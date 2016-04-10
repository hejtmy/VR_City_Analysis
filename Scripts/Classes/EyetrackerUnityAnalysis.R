library('R6')
source(paste(getwd(),"Scripts/Classes/BaseUnityAnalysis.R",sep="/"))
source(paste(getwd(),"Scripts/HelperFunctions/preprocess_functions.R",sep="/"))
source(paste(getwd(),"Scripts/HelperFunctions/analysis_functions.R",sep="/"))
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
        exp_log = NULL,
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
    SetParticipant = function(id=""){
     self$id = id
    },
    SetDataDir=function(dir=""){
     self$dir = dir
    },
    ReadData = function(){
      private$read_data_private()
    },
    
    MakePathImage = function(path = ""){
      if (nargs() >= 1){
           make_path_image(img_location = path, position_table = self$position_table)
           
      } else {
           make_path_image(img_location = self$exp_log$terrain$Map_image_path, position_table = self$position_table)
      }
    }
    ),
    
    private = list(
      is_valid = function(){
        #example
        #if((object@d < 0) || (object@y < 0)) {
        #     return("A negative number for one of the coordinates was given.")
        #}
        #return(TRUE)
        if (is.null(self$exp_log)) return(FALSE)
        if (is.null(self$position_table)) return(FALSE)
      },
      set_session_task_directory = function(){
        self$session_task_dir <- paste(self$dir,self$id,"VR",self$session,self$task,sep="/")
      },
      read_data_private = function(){
        #session/task folder
        private$set_session_task_directory()
        
        #open_player_log is a function in preprocess_functions.R
        #takes four arguments: directory whre the logs are located, 
        #patients id and session and task of the experiment
        self$position_table <- OpenPlayerLog(self$session_task_dir)
        
        #open_experiment_log is a function in preprocess_functions.R
        #takes three arguments: directory whre the logs are located, 

        #patients id and session and task of the experiment
        self$exp_log <- OpenExperimentLog(self$session_task_dir)
        
        self$scenario_log = OpenQuestLog(self$session_task_dir, self$exp_log$scenario$Name, self$exp_log$scenario$Timestamp)
        
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
        }
    )
)

OpenPlayerLog <- function(dir = ""){

     logs = list.files(dir, pattern = "_player_" ,full.names = T)
     if (length(logs)>1){
          print("There is more player logs in the same folder. HAve you named and stored everything appropriately?")
          return(NULL)
     }
     log = logs[1]
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
     pos_tab <- read.table(log, header=T,sep=";",dec=".", skip=idxBottom, stringsAsFactors=F)
     
     #deletes the last column - it's there for the easier logging from unity
     pos_tab[,length(names(pos_tab))]=NULL
     
     pos_tab <- vector3_to_columns(pos_tab,"Position")
     return(pos_tab)
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
     ls[["steps"]]  <- read.table(textConnection(text[(idxStepTop+1):(idxStepBottom-1)]),header=T,sep=";")
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

