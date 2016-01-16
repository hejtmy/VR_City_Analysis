source("preprocess_functions.R")
source("analysis_functions.R")

VR_analysis <- setClass(
     #sets the name
     "VR_analysis",
     
     #define variables
     representation = list(
          #basic definitions
          dir = "character",
          code = "character",
          session ="character",
          task="character",
          session_task_dir = "character",
          
          #loaded tables and lists
          exp_log = "list",
          pos_table = "data.frame",
          scenario_log = "list",
          quests_log = "list"
     ),
     
     #default values
     prototype = list(
          dir = paste(getwd()),
          session = "Session1",
          task = "Task1",
          session_task_dir = NULL,
          exp_log = NULL,
          pos_table = NULL,
          scenario_log = NULL,
          quests_log = NULL
          
     ),
     #define what is valid in the current context
     validity = function(object){
          #example
          #if((object@d < 0) || (object@y < 0)) {
          #     return("A negative number for one of the coordinates was given.")
          #}
          #return(TRUE)
          if (is.null(exp_log)) return(FALSE)
          if (is.null(pos_table)) return(FALSE)
          
     }
)

setMethod(f = "initialize",
          signature = "VR_analysis", 
          function(.Object, dir="", code="", session="",task=""){
               if(nargs() > 1) {
                    #if(length(x) != length(y))
                    #     stop("specified x and y of different lengths")
                    .Object@code <- code
                    .Object@dir <- paste(dir,code,sep="/",collapse=NULL)
               }
               if(nargs() >= 4) {
                    #session/task folder
                    .Object@session_task_dir = paste(.Object@dir,code,session,task,sep="/");
                    
                    #open_player_log is a function in preprocess_functions.R
                    #takes four arguments: directory whre the logs are located, 
                    #patients code and session and task of the experiment
                    .Object@pos_table <- OpenPlayerLog(.Object@session_task_dir)
                    
                    #open_experiment_log is a function in preprocess_functions.R
                    #takes three arguments: directory whre the logs are located, 
                    #patients code and session and task of the experiment
                    .Object@exp_log <- OpenExperimentLog(.Object@session_task_dir)
                    
                    
                    .Object@scenario_log <- OpenQuestLog(.Object@session_task_dir,.Object@code,.Object@exp_log$scenario$Name,.Object@exp_log$scenario$Timestamp)
                    #if we opened scenario log, we open all appropriate quest logs from the scenario
                    if(!is.null(.Object@scenario_log)){
                         ls = list()
                         #list of activated logs from the scenario process
                         table_steps_activated <- .Object@scenario_log$data[.Object@scenario_log$data$Action=="StepActivated",]
                         for(i in 1:nrow(table_steps_activated)){
                              ##MIGHT HAVE TO CHANGE IT A BIT BECAUSE OF TASK GROUPS
                              step = table_steps_activated[i,]
                              timestamp = step$Timestamp
                              #name of the step that activated the quest
                              activatingStepName = .Object@scenario_log$steps[.Object@scenario_log$steps$ID == step$StepID,"Name"]
                              #get the name of the quest activated from the name of the atctivation step
                              quest_name <- GetActivatedQuestName(activatingStepName)
                              ls[[quest_name]]<-OpenQuestLog(.Object@dir,.Object@code,quest_name,timestamp)
                         }
                         .Object@quests_log <- ls
                    }
               }
               .Object
          }
)

setGeneric("SetSession", function(object,number,...) standardGeneric("SetSession"))
setMethod(f = "SetSession",
          signature(object = "VR_analysis",number="numeric"),
          function(object, number,..){
               object@session = paste("Session",number,sep="")
               return(object)
          }
)

setGeneric("SetTask", function(object,number,...) standardGeneric("SetTask"))
setMethod(f = "SetTask",
          signature(object = "VR_analysis",number="numeric"),
          function(object, number,..){
               object@task = paste("Task",number,sep="")
               return(object)
          }
)


setGeneric("SetExperiment", function(object,timestamp,...) standardGeneric("SetExperiment"))

setMethod(f = "SetExperiment",
          signature(object = "VR_analysis",timestamp ="character"),
          function(object, timestamp,..){
               object@timestamp = timestamp
               return(object)
          }
)

setGeneric("SetParticipant",function(object,code,...) standardGeneric("SetParticipant"))

setMethod("SetParticipant",
          signature(object = "VR_analysis",code ="character"),
          function(object,code,...){
               object@code = code
               return(object)
          }
)    

setGeneric("GetParticipant",function(object) standardGeneric("GetParticipant"))

setMethod("GetParticipant",
          signature(object = "VR_analysis"),
          function(object){
               return(object@code)
          }
)  

setGeneric ("MakePathImage",function(object,...) standardGeneric("MakePathImage"))
#custom made for now
setMethod("MakePathImage",
          signature = "VR_analysis",
          function(object,path = "",...){
               if (nargs() > 1){
                    make_path_image(img_location = path,position_table = object@pos_table)
                    
               } else {
                    make_path_image(img_location = object@exp_log$terrain$Map_image_path,position_table = object@pos_table)
                    
               }
                    
          }
)

setGeneric ("SumTime",function(object,...) standardGeneric("SumTime"))

setMethod("SumTime",
          signature = "VR_analysis",
          function(object,...){
               calculate_cululative_time()
          }
)

OpenPlayerLog <- function(dir = ""){

     logs = list.files(dir, "_player_" = ptr ,full.names = T)
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

OpenExperimentLog <- function(dir = "", patient_code ="", date_time = ""){
     
     ls = list()

     #needs to check if we got only one file out
     logs = list.files(dir, "_experiment_" = ptr,full.names = T)
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

OpenQuestLog <- function(dir = "", patient_code ="",  name = "", date_time = ""){
     
     ls = list()
     ptr <- paste(patient_code, "_", escapeRegex(name), "_", date_time, "*.txt$", sep="")
     
     #needs to check if we got only one file out
     log = list.files(dir, pattern = ptr,full.names = T)[1]
     
     #if the file does not exists returning NULL and exiting
     if(!file.exists(log)){
          print("Could not find the file for given quest log")
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

