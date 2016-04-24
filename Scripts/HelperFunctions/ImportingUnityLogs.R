### Reading the data in 
OpenExperimentLogs = function(directory = ""){
  ls = list()
  #needs to check if we got only one file out
  logs = list.files(directory, pattern = "_experiment_",full.names = T)
  if(length(logs) < 1){
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
  ptr = paste(experiment_log$header$Patient, "_player_", experiment_log$header$Time, sep="", collapse="")
  logs = list.files(directory, pattern = ptr, full.names = T)
  log_columns_types = c(Time="numeric",Position="numeric",Rotation.X="numeric",Rotation.Y="numeric", Focus = "character", FPS = "numeric", Input="character")
  preprocessed_log_column_types = c(log_columns_types, Position.x="numeric", Position.y="numeric", Position.z="numeric",distance="numeric",cumulative_distance="numeric")
  if(length(logs)<1){
    SmartPrint(c("Could not find the file for player log", ptr))
    return(NULL)
  }
  if (length(logs)>1){
    #check if there is a preprocessed player file
    preprocessed_index = grep("*_preprocessed",logs)
    if(length(preprocessed_index) >0){
      if(override){
        SmartPrint(c("Removing preprocessed log", ptr))
        log = logs[1]
        file.remove(logs[preprocessed_index])
      } else {
        SmartPrint(c("Loading preprocessed player log", ptr))
        log = logs[preprocessed_index]
        return(fread(log, header=T, sep=";",dec=".", stringsAsFactors = F, colClasses = preprocessed_log_column_types))
      }
    } else{
      print("There is more player logs with appropriate timestamp in the same folder. Have you named and stored everything appropriately?")
      return(NULL)
    }
  } else {
    log = logs[1]
  }
  SmartPrint(c("Loading unprocessed player log", ptr))
  
  #reads into a text file at first
  text = readLines(log,warn=F)
  
  #finds the header start
  idxTop <- which(grepl('\\*\\*\\*\\*\\*',text))
  #finds the header bottom
  idxBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
  #potentially returns the header as well in a list
  #todo
  
  #reads the data without the header file
  pos_tab <- fread(log, header=T, sep=";", dec=".", skip=idxBottom, stringsAsFactors=F, colClasses = log_columns_types)
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
  if (changed) print("Log modified") else print("Log ok")
  return(changed)
}
SavePreprocessedPlayer = function(experiment_log, pos_tab){
  directory = dirname(experiment_log$filename)
  ptr = paste("_player_", experiment_log$header$Time, sep="", collapse="")
  log = list.files(directory, pattern = ptr ,full.names = T)[1]
  #writes preprocessed file
  preprocessed_filename = gsub(".txt","_preprocessed.txt",log)
  SmartPrint(c("Saving processed player log as", preprocessed_filename))
  write.table(pos_tab, preprocessed_filename, sep=";", dec=".", quote=F, row.names = F)
}
OpenScenarioLog = function(experiment_log){
  directory = dirname(experiment_log$filename)
  ptr <- paste("_", escapeRegex(experiment_log$scenario$Name), "_", experiment_log$scenario$Timestamp, "*.txt$", sep="")
  #needs to check if we got only one file out
  log = list.files(directory, pattern = ptr, full.names = T)[1]
  #if the file does not exists returning NULL and exiting
  if(!file.exists(log)){
    print(paste("Could not find the file for scenario log", ptr, sep = " "))
    print(ptr)
    return(NULL)
  }
  scenario_log = OpenQuestLog(log)
  return(scenario_log)
}
OpenQuestLogs = function(experiment_log, scenario_log = NULL){
  if(!is.null(scenario_log)){
    directory = dirname(experiment_log$filename)
    #prepares list
    ls = list()
    #list of activated logs from the scenario process
    #it looks for steps finished because for some weird reason of bad logging
    table_steps_activated <- scenario_log$data[scenario_log$data$Action=="StepActivated",]
    table_steps_finished <- scenario_log$data[scenario_log$data$Action=="StepFinished",]
    if (nrow(table_steps_activated) >= nrow(table_steps_finished)) use_finished = F else use_finished = T
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
      if(is.na(quest_name)) next
      if (!is.null(quest_name) ){
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
  file = textConnection(text[(idxStepTop+1):(idxStepBottom-1)])
  ls[["steps"]]  <- read.table(file,header=T,sep=";",stringsAsFactors=F)
  close(file)
  #and the timestamps and other the the data list
  ls[["data"]] <- read.table(filepath, header=T, sep=";",dec=".", skip=idxStepBottom, stringsAsFactors=F)
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
