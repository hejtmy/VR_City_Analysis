open_player_log =  function(experiment_log, override = F){
  directory = dirname(experiment_log$filename)
  ptr = paste(experiment_log$header$Patient, "_player_", experiment_log$header$Time, sep="", collapse="")
  logs = list.files(directory, pattern = ptr, full.names = T)
  log_columns_types = c(Time="numeric",Position="numeric",Rotation.X="numeric",Rotation.Y="numeric", Focus = "character", FPS = "numeric", Input="character")
  preprocessed_log_column_types = c(log_columns_types, Position.x="numeric", Position.y="numeric", Position.z="numeric",distance="numeric",cumulative_distance="numeric")
  if(length(logs) < 1){
    SmartPrint(c("!!!Could not find the file for player log!!!", ptr))
    return(NULL)
  }
  if (length(logs)>1){
    #check if there is a preprocessed player file
    preprocessed_index = grep("*_preprocessed",logs)
    if(length(preprocessed_index) > 0){
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