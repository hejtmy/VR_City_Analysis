prep_eye_events = function(tab){
  rm_brackets = function(x) gsub("\\[|\\]", "", x)
  tab[, type := sapply(type, rm_brackets)]
  tab = tab[type == "l"]
  return(tab)
}

