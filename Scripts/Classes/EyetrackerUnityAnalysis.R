library('R6')
library('data.table')
source(paste(getwd(),"Scripts/Classes/BaseUnityAnalysis.R",sep="/"))
source(paste(getwd(),"Scripts/HelperFunctions/helper_functions.R",sep="/"))
source_folder(paste(getwd(),"Scripts/HelperFunctions/",sep="/"))

data_path = "/Data"
UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
        #basic definitions
        session = NULL,
        task= NULL,
        session_task_dir = NULL,
        
        #loaded tables and lists
        experiment_log = NULL,
        position_table = NULL,
        scenario_log = NULL,
        quests_log = NULL,
        
    initialize = function(dir=data_path, id="", session=NULL, task=NULL){
       self$dir = dir
       self$SetParticipant(id)
       self$SetSession(session)
       self$SetTask(task)
       
       #TODO - check the data
       if(nargs() >= 4) {
          self$read_data_private()
       }
    },
    
    #define what is valid in the current context
    SetSession = function(number=1){
     self$session = paste("Session",number,sep="")
    },
    SetTask = function(number=1){
     self$task = paste("Task",number,sep="")
    },
    ReadData = function(override = F, save = T){
      private$read_data_private(override, save)
    },
    
    # Makes a graph with a path from start to finish
    MakePathImage = function(path = "", quest_idx = 0){
      map_img_location = ""
      if (!missing(path)){
        map_img_location = path
      } else {
        map_img_location = self$experiment_log$terrain$Map_image_path
      }
      if (quest_idx == 0){
        path_table = self$position_table
        return(make_path_image(img_location = map_img_location, position_table = path_table))
      } else {
        time_window = private$get_quest_timewindow(quest_idx)
        if(!is.null(time_window)){
          path_table = private$select_position_data(time_window)
        }
        special_paths = list()
        special_paths[["teleport"]]= private$get_teleport_times(quest_idx)
        quest_start_and_stop = private$get_start_and_finish_positions(quest_idx)

        map_size = GetMapSize(self$experiment_log$terrain)
        
        if (!is.null(special_paths)){
          make_path_image(img_location = map_img_location, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
        } else {
          make_path_image(img_location = map_img_location, position_table = path_table, map_size = map_size)
        }
      }
    },
    QuestSummary = function(quest_idx = 0){
      ls = list()
      quest = self$quests_log[quest_idx][[1]]
      
      quest_times = private$get_quest_timewindow(quest_idx, include_teleport = F)
      ls[["Time"]] =  diff(c(quest_times$start,quest_times$finish))
      #distance
      positions = c(self$position_table[Time > quest_times$start, .SD[1,cumulative_distance]],self$position_table[Time > quest_times$finish, .SD[1,cumulative_distance]])
      ls[["Distance"]] = diff(positions)
      return(ls)
    }
    ),
    
    private = list(
      is_valid = function(){
        if (is.null(self$experiment_log)) return(FALSE)
        if (is.null(self$position_table)) return(FALSE)
      },
      set_session_task_directory = function(){
        self$session_task_dir <- paste(self$dir,self$id,"VR",self$session,self$task,sep="/")
      },
      read_data_private = function(override, save){
        #session/task folder
        private$set_session_task_directory()
        
        #open_player_log is a function in preprocess_functions.R
        #takes four arguments: directory whre the logs are located, 
        #patients id and session and task of the experiment
        self$position_table = OpenPlayerLog(self$session_task_dir, override)
        
        #checks if there is everything we need and if not, recomputes the stuff
        changed = PreprocessPlayerLog(self$position_table)
        if (changed & save) {
          SavePreprocessedPlayer(self$session_task_dir, self$position_table)
        }
        #open_experiment_log is a function in preprocess_functions.R
        #takes three arguments: directory whre the logs are located, 

        #patients id and session and task of the experiment
        self$experiment_log <- OpenExperimentLog(self$session_task_dir)
        
        self$scenario_log <- OpenQuestLog(self$session_task_dir, self$experiment_log$scenario$Name, self$experiment_log$scenario$Timestamp)
        
        #if we opened scenario log, we open all appropriate quest logs from the scenario
        if(!is.null(self$scenario_log)){
         ls = list()
         #list of activated logs from the scenario process
         table_steps_activated <- self$scenario_log$data[self$scenario_log$data$Action=="StepActivated",]
         for(i in 1:nrow(table_steps_activated)){
           ##MIGHT HAVE TO CHANGE IT A BIT BECAUSE OF TASK GROUPS
           step = table_steps_activated[i,]
           timestamp = step$Timestamp
           #name of the step that activated the quest
           activatingStepName = self$scenario_log$steps[self$scenario_log$steps$ID == step$StepID,"Name"]
           #get the name of the quest activated from the name of the atctivation step
           quest_name <- GetActivatedQuestName(activatingStepName)
           if (!is.na(quest_name)){
             ls[[quest_name]] <- OpenQuestLog(self$session_task_dir, quest_name, timestamp)
           }
         }
         self$quests_log <- ls
        }
        private$is_valid()
        },
      select_position_data = function(time_window){
        if(missing(time_window)){
          stop("Need to specify time window")
        }
        if (length(time_window)!=2){
          stop("Time window needs to have only two times inside")
        }
        return(self$position_table[Time>time_window$start & Time < time_window$finish])
      },
      get_quest_timewindow = function(quest_idx, include_teleport = T){
        if(missing(quest_idx)){
          stop("Need to specify the quest index")
        }
        quest = self$quests_log[quest_idx][[1]]
        if(is.null(quest)){
          stop("Quest log not reachable")
        }
        if (include_teleport){
          start_time = quest$data$TimeFromStart[quest$data$Action == "Quest started"]
        } else{
          start_time = private$get_teleport_times(quest_idx)$finish
        }
        end_time = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
        ls = list()
        ls[["start"]] = start_time
        ls[["finish"]] = end_time
        return(ls)
      },
      get_teleport_times = function(quest_idx){
        quest = self$quests_log[quest_idx][[1]]
        if(is.null(quest)){
          stop("Quest log not reachable")
        }
        teleport_start_time = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" & quest$data$Action =="StepActivated"]
        teleport_finish_time = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" & quest$data$Action == "StepFinished"]
        ls = list()
        ls[["start"]] = teleport_start_time
        ls[["finish"]] = teleport_finish_time
        return(ls)
      },
      get_start_and_finish_positions = function(quest_idx, include_teleport = T){
        quest = self$quests_log[quest_idx][[1]]
        if(is.null(quest)) stop("Quest log not reachable")
        #gets finished time of the teleport
        teleport_finished = private$get_teleport_times(quest_idx)$finish
        teleport_target_postition = self$position_table[Time > teleport_finished, .SD[1,c(Position.x,Position.z)]]
        #goes step by step from the end until it gets end with transform
        for(i in nrow(quest$steps):1){
          quest_finish_position = text_to_vector3(quest$steps$Transform[i])
          if(!is.null(quest_finish_position)){
            quest_finish_position = quest_finish_position[c(1,3)]
            break
          }
        }
        ls = list()
        ls[["start"]] = teleport_target_postition
        ls[["finish"]] = quest_finish_position
        return(ls)
      },
      get_step_time = function(quest_idx, step_name, step_action = "StepActivated", step_id = 0){
        quest = self$quests_log[quest_idx][[1]]
        if(is.null(quest)){
          stop("Quest log not reachable")
        }
        if(step_id != 0){
          return(quest$data$TimeFromStart[quest$data$StepID == quest_idx & quest$data$Action == step_action])
        }
        return(quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action])
      }
    )
)

OpenPlayerLog <- function(dir = "", override = F){
     logs = list.files(dir, pattern = "_player_" ,full.names = T)
     if (length(logs)>1){
       #check if there is a preprocessed player file
       preprocessed_index = grep("*_preprocessed",logs)
       if(length(preprocessed_index)>0){
         if(override){
           log = logs[1]
           file.remove(logs[preprocessed_index])
         } else {
           log = logs[preprocessed_index]
           return(fread(log, header=T, sep=";",dec=".", stringsAsFactors = F))
         }
       }else{
          print("There is more player logs in the same folder. HAve you named and stored everything appropriately?")
          return(NULL)
       }
     } else {
       log = logs[1]
     }
     
     if(!file.exists(log)){
        print("Could not find the file for player log")
        return(NULL)
     }
     
     #reads into a text file at first
     text = readLines(log,warn=F)
     
     #finds the header start
     idxTop <- which(grepl('\\*\\*\\*\\*\\*',text))
     #finds the header bottom
     idxBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
     #potentially returns the header as well in a list
     #todo
     
     #reads the data without the header file
     pos_tab <- fread(log, header=T, sep=";", dec=".", skip=idxBottom, stringsAsFactors=F)
     #deletes the last column - it's there for the easier logging from unity 
     # - its here because of how preprocessing works
     pos_tab[,ncol(pos_tab):=NULL]
     
     return(pos_tab)
}
PreprocessPlayerLog = function(pos_tab){
  #check_stuff
  #check columns
  changed = F
  if (!ColumnPresent(colnames(pos_tab),"Position.x")){
    pos_tab = vector3_to_columns(pos_tab,"Position")
    changed = T
  }
  if (!ColumnPresent(colnames(pos_tab),"cumulative_distance")){
    pos_tab = AddDistanceWalked (pos_tab)
    changed = T
  }
  return(changed)
}
SavePreprocessedPlayer = function(dir = "", pos_tab){
  logs = list.files(dir, pattern = "_player_" ,full.names = T)
  log = logs[1]
  #writes preprocessed file
  preprocessed_filename = gsub(".txt","_preprocessed.txt",log)
  write.table(pos_tab, preprocessed_filename, sep=";", dec=".", quote=F, row.names = F)
}
OpenExperimentLog <- function(dir = ""){
     
     ls = list()

     #needs to check if we got only one file out
     logs = list.files(dir, pattern = "_experiment_",full.names = T)
     if (length(logs)>1){
          print("There is more player logs in the same folder. HAve you named and stored everything appropriately?")
          return(NULL)
     }
     log = logs[1]
     if(!file.exists(log)){
          print("Could not find the file for experiment log")
          return(NULL)
     }
     #reads into a text file at first
     text = readLines(log,warn=F)
     #finds the header start
     idxHeaderTop <- which(grepl('\\*\\*\\*\\*\\*',text))
     #finds the header bottom
     idxHeaderBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
     #potentially returns the header as well in a list
     ls[["header"]] <- into_list(text[(idxHeaderTop+1):(idxHeaderBottom-1)])
     
     #todo
     idxTerrainTop <- which(grepl('\\*\\*\\*Terrain information\\*\\*\\*',text))
     idxTerrainBottom <- which(grepl('\\-\\-\\-Terrain information\\-\\-\\-',text))
     ls[["terrain"]]  <- into_list(text[(idxTerrainTop+1):(idxTerrainBottom-1)])
     
     #todo - so far it only reads one
     idxSceneTop <- which(grepl('\\*\\*\\*Scenario information\\*\\*\\*',text))
     idxSceneBottom <- which(grepl('\\-\\-\\-Scenario information\\-\\-\\-',text))
     ls[["scenario"]]  <- into_list(text[(idxSceneTop+1):(idxSceneBottom-1)])
     
     return(ls)     
}
OpenQuestLog <- function(task_dir = "",  name = "", date_time = ""){
     
     ls = list()
     ptr <- paste("_", escapeRegex(name), "_", date_time, "*.txt$", sep="")
     
     #needs to check if we got only one file out
     log = list.files(task_dir, pattern = ptr, full.names = T)[1]
     
     #if the file does not exists returning NULL and exiting
     if(!file.exists(log)){
          print(paste("Could not find the file for given quest log", name, date_time, sep = " "))
          print(ptr)
          return(NULL)
     }
     #reads into a text file at first
     text = readLines(log,warn=F)
     #finds the header start
     idxHeaderTop <- which(grepl('\\*\\*\\*\\*\\*',text))
     #finds the header bottom
     idxHeaderBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
     #potentially returns the header as well in a list
     ls[["header"]] <- into_list(text[(idxHeaderTop+1):(idxHeaderBottom-1)])
     
     #todo - reads the header 
     idxStepTop <- which(grepl('\\*\\*\\*Quest step data\\*\\*\\*',text))
     idxStepBottom <- which(grepl('\\-\\-\\-Quest step data\\-\\-\\-',text))
     #puts everyting from the quest header to the steps list
     ls[["steps"]]  <- read.table(textConnection(text[(idxStepTop+1):(idxStepBottom-1)]),header=T,sep=";",stringsAsFactors=F)
     #and the timestamps and other the the data list
     ls[["data"]] <- read.table(textConnection(text), header=T, sep=";",dec=".", skip=idxStepBottom, stringsAsFactors=F)
     return(ls)     
}
#helper function to figure out the name of the activated quest as is saved in the steps
#list in the scenario quest
GetActivatedQuestName <- function(string =""){
     #The name of the quest is between square brackets - [quest name]
     name <- str_extract_all(string,"\\[(.*?)\\]")[[1]][1]
     #removing the square brackets
     name <- substring(name,2,nchar(name)-1)
     return(name)
}

GetMapSize = function(terrain_info){
  ls = list()
  size = text_to_vector3(terrain_info$Size)
  pivot = text_to_vector3(terrain_info$Pivot)
  ls[["x"]] = c(pivot[1],pivot[1] + size[1])
  ls[["y"]] = c(pivot[3],pivot[3] + size[3])
  return(ls)
}

