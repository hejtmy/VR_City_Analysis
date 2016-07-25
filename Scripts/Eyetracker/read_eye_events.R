read_eye_events = function(text){
  EVENT_NAMES = c("KEY_UP", "KEY_DOWN")
  MSG_indices = grep('^MSG\\t+.*', text)
  lines = text[MSG_indices]
  event_indices = sapply(lines, contains_word, EVENT_NAMES)
  lines = lines[event_indices]
  #removing the MSG part
  lines = gsub('^MSG\t', '', lines, perl=T)
  #creates one file with each char on a single line
  text = paste(lines, sep="", collapse="\n")
  tab = fread(text, sep = " ", header = F)
  tab[,c("V2", "V4", "V5", "V6"):=NULL]
  colnames(tab) = c("time", "name", "type")
  return(tab)
}