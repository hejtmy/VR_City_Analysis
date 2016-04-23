data_path = "/Data"

UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
        #basic definitions
        session = NULL,
        session_dir = NULL,
        
        trial_sets = NULL,
        quest_set = NULL,
    initialize = function(dir=data_path, id="", session=NULL){
       self$dir = dir
       self$SetParticipant(id)
       self$SetSession(session)
       
       #TODO - check the data
       if(nargs() >= 4) {
          private$read_data_private()
       }
    },
    
    #define what is valid in the current context
    SetSession = function(number=1){
     self$session = paste("Session",number,sep="")
    },
    ReadData = function(override = F, save = T){
      private$read_data_private(override, save)
    },
    
    # Makes a graph with a path from start to finish
    MakePathImage = function(quest_session_idx = NULL, img_path = "Maps/megamap5.png"){
      #Hmakes the path for the entire thing
      if (is.null(quest_session_idx)){
        path_table = private$PlayerLog()
        map_size = private$MapSize()
        return(make_path_image(img_location = img_path, position_table = path_table, map_size = map_size))
      } else {
        quest = private$QuestStep(quest_session_idx)
        time_window = private$get_quest_timewindow(quest)
        if(!is.null(time_window)){
          path_table = private$select_position_data(quest,time_window)
        }
        special_paths = list()
        special_paths[["teleport"]]= private$get_teleport_times(quest)
        quest_start_and_stop = private$get_start_and_finish_positions(quest)
        map_size = private$MapSize(quest)
        if (!is.null(special_paths)){
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
        } else {
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size)
        }
      }
    },
    DrawQuestParth = function(quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
      special_paths = list()
      quest_start_and_Stop = NULL
      path_table = data.table()
      
      for(i in 1:length(types)){
        type = types[i]
        #get the session quest_idx
        quest_session_id = private$getQuestSessionId(quest_id, type)
        quest = private$QuestStep(quest_session_id)
        
        if (i == 1){
          quest_start_and_stop = private$get_start_and_finish_positions(quest)
          map_size = private$MapSize(quest)
        }
        time_window = private$get_quest_timewindow(quest, include_teleport = F)
        special_paths[[type]] = time_window
        #adds path_table to the 
        quest_path_table = private$select_position_data(quest,time_window)
        path_table = rbindlist(list(path_table,quest_path_table))
      }
      
      make_path_image(img_location = img_path, position_table = path_table, map_size = map_size,special_paths = special_paths, special_points = quest_start_and_stop)
    },
    QuestsSummary = function(){
      df = self$quest_set
      trail_times = numeric(nrow(df))
      trail_distances = numeric(nrow(df))
      for(i in 1:nrow(df)){
        quest_summary = self$QuestSummary(quest_session_idx = i)
        trail_times[i] = quest_summary$Time
        trail_distances[i] = quest_summary$Distance
      }
      df = mutate(df, time = trail_times, distance = trail_distances)
      return(df)
    },
    #takes either quest_id or session id as a parameter
    QuestSummary = function(quest_idx = NULL, quest_session_idx = NULL){
      ls = list()
      if (is.null(quest_idx)){
        quest = private$QuestStep(quest_session_idx)
        quest_times = private$get_quest_timewindow(quest, include_teleport = F)
        ls$Time =  diff(c(quest_times$start,quest_times$finish))
        player_log = private$PlayerLogForQuest(quest)
        #needs to find the first place after the teleport
        positions = c(player_log[Time > quest_times$start, .SD[1,cumulative_distance]],tail(player_log,1)$cumulative_distance)
        ls$Distance = diff(positions)
      } 
      if (is.null(quest_session_idx)){
        quest_types = c("learn","trial")
        quests = private$QuestStep(quest_idx, quest_types)
        #for each list member - checking if there are two
        for(type in quest_types){
          quest = quests[[type]]
          quest_times = private$get_quest_timewindow(quest, include_teleport = F)
          ls[[type]]$Time = diff(c(quest_times$start, quest_times$finish))
          player_log = private$PlayerLogForQuest(quest)
          #needs to find the first place after the teleport
          positions = c(player_log[Time > quest_times$start, .SD[1,cumulative_distance]],tail(player_log,1)$cumulative_distance)
          ls[[type]]$Distace = diff(positions)
        }
      }
      return(ls)
    },
    PublicQuestStep = function(quest_idx, quest_types = NULL){
      private$QuestStep(quest_idx, quest_types)
      }
    ),
    private = list(
      is_valid = function(){
        if (is.null(self$experiment_log)) return(FALSE)
        if (is.null(self$position_table)) return(FALSE)
      },
      set_session_directory = function(){
        self$session_dir <- paste(self$dir,self$id,"VR",self$session,sep="/")
      },
      read_data_private = function(override, save){
        #session folder
        private$set_session_directory()
        
        #open experiment_logs to see how many do we have
        experiment_logs = OpenExperimentLogs(self$session_dir)
        
        #for each experiment_log, we open player log, scenario log and appropriate quest logs
        self$trial_sets = list()
        for (i in 1:length(experiment_logs)){
          experiment_log = experiment_logs[[i]]
          player_log = OpenPlayerLog(experiment_log, override)
          #preprocesses player log
          #checks if there is everything we need and if not, recomputes the stuff
          changed = PreprocessPlayerLog(player_log)
          if (changed & save) {
            SavePreprocessedPlayer(experiment_log, player_log)
          }
          scenario_log = OpenScenarioLog(experiment_log)
          quests_logs = OpenQuestLogs(experiment_log, scenario_log)
          
          self$trial_sets[[i]] = UnityTrialSet$new(experiment_log, player_log, scenario_log, quests_logs)
        }
        self$quest_set = MakeQuestTable(self$trial_sets)
        private$is_valid()
      },
      getQuestSessionId = function(quest_id, quest_type){
        quest_session_id = (filter(self$quest_set,id == quest_id & type == quest_type) %>% select(session_id))[[1]]
        if (length(quest_session_id) > 1) stop("There are more quests with this id. Do you have correct logs in the directory?")
        return(quest_session_id)
      },
      select_position_data = function(quest,time_window){
        if(missing(time_window)) stop("Need to specify time window")
        if (length(time_window)!=2) stop("Time window needs to have only two times inside")
        id_of_set = (filter(self$quest_set, name == quest$name) %>% select(id_of_set))[[1]]
        
        position_table = self$trial_sets[[id_of_set]]$player_log
        return(position_table[Time > time_window$start & Time < time_window$finish])
      },
      get_quest_timewindow = function(quest = NULL, quest_idx = NULL, include_teleport = T){
        if(is.null(quest)) quest = private$QuestStep(quest_idx)
        if(is.null(quest)) stop("Quest log not reachable")
        if(include_teleport){
          start_time = quest$data$TimeFromStart[quest$data$Action == "Quest started"]
        }else{
          start_time = private$get_teleport_times(quest)$finish
        }
        end_time = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
        ls = list()
        ls[["start"]] = start_time
        ls[["finish"]] = end_time
        return(ls)
      },
      get_teleport_times = function(quest = NULL, quest_idx=NULL){
        if(is.null(quest)) quest = private$QuestStep(quest_idx)
        if(is.null(quest)) stop("Quest log not reachable")
        teleport_start_time = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" & quest$data$Action =="StepActivated"]
        teleport_finish_time = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" & quest$data$Action == "StepFinished"]
        ls = list()
        ls[["start"]] = teleport_start_time
        ls[["finish"]] = teleport_finish_time
        return(ls)
      },
      get_start_and_finish_positions = function(quest, include_teleport = T){
        if(is.null(quest)) stop("Quest log not reachable")
        #gets finished time of the teleport
        teleport_finished = private$get_teleport_times(quest)$finish
        teleport_target_postition = private$PlayerLogForQuest(quest)[Time > teleport_finished, .SD[1,c(Position.x,Position.z)]]
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
        quest = private$QuestStep(quest_idx)
        if(is.null(quest))stop("Quest log not reachable")
        if(step_id != 0) return(quest$data$TimeFromStart[quest$data$StepID == quest_idx & quest$data$Action == step_action])
        return(quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action])
      },
      QuestStep = function(quest_idx, quest_types = NULL){
        ls = list()
        #if the length is 0, we assume that the quest_idx is quest_session_idx
        if (length(quest_types) == 0){
          quest_lines = filter(self$quest_set, session_id %in% quest_idx)
          if(nrow(quest_lines) == 0) return(NULL);
          #foreach
          for(i in 1:nrow(quest_lines)){
            quest_line = quest_lines[i]
            quest = self$trial_sets[[quest_line$id_of_set]]$quest_logs[quest_line$set_id]
            quest[[1]]$name = select(quest_line,name)[[1]]
            ls = c(ls,quest)
          }
          #$removes redundant header - we can resave it
          ls = ls[[1]]
        } 
        if(length(quest_types) > 0){
          quest_session_id  = filter(self$quest_set, id == quest_idx & type %in% quest_types) %>% select(session_id)
          quest_lines = filter(self$quest_set, session_id %in% quest_session_id[[1]])
          #foreach
          for(i in 1:nrow(quest_lines)){
            quest_line = quest_lines[i]
            ls[[quest_types[i]]] = self$trial_sets[[quest_line$id_of_set]]$quest_logs[quest_line$set_id][[1]]
            ls[[quest_types[i]]]$name = select(quest_line,name)[[1]]
          }
        }
        return(ls)
      },
      PlayerLog=function(){
        player_log = data.table()
        for(i in 1:length(self$trial_sets)){
          pos_tab =  self$trial_sets[[i]]$player_log
          player_log = rbindlist(list(player_log,pos_tab))
        }
        return(player_log)
      },
      PlayerLogForQuest = function(quest){
        quest_line = filter(self$quest_set, name == quest$name)
        quest_times = private$get_quest_timewindow(quest, include_teleport = T)
        player_log = self$trial_sets[[quest_line$id_of_set]]$player_log[Time > quest_times$start & Time <quest_times$finish,]
        return(player_log)
      },
      MapSize = function(quest = NULL){
        ls = list()
        if (is.null(quest)) quest = private$QuestStep(1)
        quest_line = filter(self$quest_set, name == quest$name)
        terrain_info = self$trial_sets[[quest_line$id_of_set]]$experiment_log$terrain
        size = text_to_vector3(terrain_info$Size)
        pivot = text_to_vector3(terrain_info$Pivot)
        ls[["x"]] = c(pivot[1],pivot[1] + size[1])
        ls[["y"]] = c(pivot[3],pivot[3] + size[3])
        return(ls)
      }
    )
)

### Reading the data in 
OpenExperimentLogs = function(directory = ""){
  ls = list()
  #needs to check if we got only one file out
  logs = list.files(directory, pattern = "_experiment_",full.names = T)
  if(!file.exists(logs)){
    print("Could not find the file for experiment log")
    return(NULL)
  }
  for(i in 1:length(logs)){
    ls[[i]] = OpenExperimentLog(logs[i])
    ls[[i]]$filename = logs[i]
  }
  return(ls)
}
OpenExperimentLog = function(filepath){
  ls = list()
  #reads into a text file at first
  text = readLines(filepath,warn=F)
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
OpenPlayerLog = function(experiment_log, override = F){
 directory = dirname(experiment_log$filename)
 ptr = paste("_player_", experiment_log$header$Time, sep="", collapse="")
 logs = list.files(directory, pattern = ptr, full.names = T)
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
      print("There is more player logs with appropriate timestamp in the same folder. Have you named and stored everything appropriately?")
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
SavePreprocessedPlayer = function(experiment_log, pos_tab){
  directory = dirname(experiment_log$filename)
  ptr = paste("_player_", experiment_log$header$Time, sep="", collapse="")
  log = list.files(directory, pattern = ptr ,full.names = T)[1]
  #writes preprocessed file
  preprocessed_filename = gsub(".txt","_preprocessed.txt",log)
  write.table(pos_tab, preprocessed_filename, sep=";", dec=".", quote=F, row.names = F)
}
OpenScenarioLog = function(experiment_log){
  directory = dirname(experiment_log$filename)
  ptr <- paste("_", escapeRegex(experiment_log$scenario$Name), "_", experiment_log$scenario$Timestamp, "*.txt$", sep="")
  #needs to check if we got only one file out
  log = list.files(directory, pattern = ptr, full.names = T)[1]
  #if the file does not exists returning NULL and exiting
  if(!file.exists(log)){
    print(paste("Could not find the file for given quest log", ptr, sep = " "))
    print(ptr)
    return(NULL)
  }
  scenario_log = OpenQuestLog(log)
  return(scenario_log)
}
OpenQuestLogs = function(experiment_log,scenario_log = NULL){
  if(!is.null(scenario_log)){
    directory = dirname(experiment_log$filename)
    #prepares list
    ls = list()
    #list of activated logs from the scenario process
    #it looks for steps finished because for some weird reason of bad logging
    table_steps_activated <- scenario_log$data[scenario_log$data$Action=="StepActivated",]
    table_steps_finished <- scenario_log$data[scenario_log$data$Action=="StepFinished",]
    if (nrow(table_steps_activated)>nrow(table_steps_finished)) use_finished = F else use_finished = T
    for_interations = if (use_finished) nrow(table_steps_finished) else nrow(table_steps_activated) 
    for(i in 1:for_interations){
      if (use_finished){
        step = table_steps_finished[i,]
        timestamp = ""
        #name of the step that activated the quest
        finished_step_name = scenario_log$steps[scenario_log$steps$ID == step$StepID,"Name"]
        #get the name of the quest activated from the name of the atctivation step
        quest_name <- GetActivatedQuestName(finished_step_name)
      } else {
        step = table_steps_activated[i,]
        timestamp = step$Timestamp
        #name of the step that activated the quest
        activated_step_name = scenario_log$steps[scenario_log$steps$ID == step$StepID,"Name"]
        #get the name of the quest activated from the name of the atctivation step
        quest_name <- GetActivatedQuestName(activated_step_name)
      }
      if (!is.null(quest_name)){
        ptr <- paste("_", escapeRegex(quest_name), "_", timestamp, sep="")
        #needs to check if we got only one file out
        log = list.files(directory, pattern = ptr, full.names = T)[1]
        if(!file.exists(log)){
          print(paste("Could not find the file for given quest log", ptr, sep = " "))
          print(ptr)
          next
        }
        #might change this 
        ls[[quest_name]] = OpenQuestLog(log)
      }
    }
    return(ls)
  }
}
OpenQuestLog = function(filepath){
  ls = list()
  #reads into a text file at first
  text = readLines(filepath,warn=F)
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
MakeQuestTable = function(trial_sets){
  dt = data.table(id = numeric(0), session_id = numeric(0), name = character(0), type=character(0), id_of_set = numeric(0), set_id = numeric(0))
  #to keep track of the number of quests
  session_id = 1
  for (n in 1:length(trial_sets)){
    quest_logs = trial_sets[[n]]$quest_logs
    num_rows = length(quest_logs)
    dt_trial = data.table(id = numeric(num_rows), session_id = numeric(num_rows), name = character(num_rows), type=character(num_rows), id_of_set = numeric(num_rows),set_id = numeric(num_rows))
    #if we pass an empty list
    if (length(quest_logs) == 0) next
    for(i in 1:length(quest_logs)){
      #needs to pass the whole thing
      quest_info = GetQuestInfo(quest_logs[i])
      dt_trial[i,] = list(as.numeric(quest_info$id), session_id, quest_info$name, quest_info$type, n, i)
      session_id = session_id +1
    }
    dt = rbindlist(list(dt,dt_trial))
  }
  return(dt)
}
GetQuestInfo = function(quest_log){
  ls = list()
  ls[["name"]] = names(quest_log)
  #gets all the letters and numbers until the dash(-) symbol
  #first is E in VR experiments, second the quest index and then the a/b version
  id_pattern = "(.*?)-"
  id_part = str_match(ls[["name"]],id_pattern)[2]
  ls[["id"]] = str_match(id_part, "\\d+")[1]
  if(is.na(id_part)){
    print("sth")
  }
  ls[["type"]] = if (str_match(id_part, "[a-b]")[1]=="a") "learn" else "trial"
  quest_log = quest_log[[1]]
  return(ls)
}
