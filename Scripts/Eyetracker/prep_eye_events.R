prep_eye_events = function(tab){
  #checks if it has already been preprocessed
  ls = list()
  ls$changed = F
  if(grepl("\\[", tab[1, type])){
    rm_brackets = function(x) gsub("\\[|\\]", "", x)
    tab[, type := sapply(type, rm_brackets)]
    ls$changed = T
  }
  if(tab[type != 'l', .N] > 0){
    ls$result = tab[name == "KEY_UP" & type == 'l']
    ls$changed = T
  }
  return(ls)
}
