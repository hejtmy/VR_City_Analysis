prep_eye_events = function(dt){
  #checks if it has already been preprocessed
  ls = list()
  return_dt = copy(dt) #needs to copy because of indices
  rm_brackets = function(x) gsub("\\[|\\]", "", x)
  return_dt = return_dt[, type := sapply(type, rm_brackets)]
  #removing key down
  return_dt = return_dt[name == "KEY_UP",]
  #removing walking keys
  
  walking_keys = c('w','a','s','d')
  return_dt = return_dt[!(type %in% walking_keys)]
  
  #removing problematic keys
  problematic_keys = c(';')
  return_dt = return_dt[!(type %in% problematic_keys)]
  
  #' all equal returns a list of differences or TRUE 
  #' if the first element is tring, there is some difference  
  ls$changed = is.character(all.equal(dt, return_dt)[1])  
  ls$result = return_dt
  return(ls)
}
