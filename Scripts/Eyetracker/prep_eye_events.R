prep_eye_events = function(tab){
  #checks if it has already been preprocessed
  ls = list()
  return_tab = tab
  ls$changed = F
  if(grepl("\\[", return_tab[1, type])){
    rm_brackets = function(x) gsub("\\[|\\]", "", x)
    return_tab = return_tab[, type := sapply(type, rm_brackets)]
    ls$changed = T
  }
  #removing key down
  if(return_tab[name == "KEY_DOWN", .N] > 0){
    return_tab = return_tab[name == "KEY_UP",]
    ls$changed = T
  }
  #removing walking keys
  walking_keys = c('w','a','s','d')
  if(return_tab[type %in% walking_keys, .N] > 0){
    return_tab = return_tab[!(type %in% walking_keys)]
    ls$changed = T
  }
  ls$result = return_tab
  return(ls)
}
