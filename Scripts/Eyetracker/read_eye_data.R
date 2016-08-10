read_eye_data = function(data_dir, file, override){
  events = NULL
  fixations = NULL
  #checks if there are already computed files
  fixations_file = paste(data_dir, file, "_fixations.txt", sep = "")
  events_file = paste(data_dir, file, "_events.txt", sep = "")
  if (override){
    if(file.exists(fixations_file)) {
      SmartPrint(c("Removing preprocessed fixations log", fixations_file))
      file.remove(fixations_file)
    }
    if(file.exists(events_file)) {
      SmartPrint(c("Removing preprocessed events log", fixations_file))
      file.remove(events_file)
    }
  } else {
    if(file.exists(fixations_file)) {
      SmartPrint(c("Loading preprocessed fixations log", fixations_file))
      fixations = fread(fixations_file, sep=";", header = T)
    }
    if(file.exists(events_file)){
      SmartPrint(c("Loading preprocessed events log", events_file))
      events = fread(events_file, sep=";", header = T)
    }
  }
  #if we are still missing some data we recompute it
  if (is.null(events) | is.null(fixations)){
    filepath = paste(data_dir, file, ".asc", sep = "")
    #check for log existance
    if(!file.exists(filepath)){
      SmartPrint(c("ERROR:read_eye_data:MissingLog,", "ID", filepath,"DESCRIPTION: THere is no log in destination"))
      return(NULL)
    }
    SmartPrint(c("Loading eyetracker log", filepath))
    text = readLines(filepath)
    if (is.null(events)){
      events = read_eye_events(text)
    }
    if (is.null(fixations)){
      fixations = read_eye_fixations(text)
    }
  }
  ls = list("events" = events, "fixations" = fixations)
  return(ls)
}