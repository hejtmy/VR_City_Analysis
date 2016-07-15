read_eye_fixations = function(text){
  FIX_idxs = grep('^EFIX.*', text)
  lines = text[FIX_idxs]
  
  #Replaces all the EFIX R/L part up to the number
  lines = gsub('^EFIX R\\s+', '', lines, perl=T)
  lines = gsub('^EFIX L\\s+', '', lines, perl=T)
  
  #creates one file with each char on a single line
  text = paste(lines, sep="", collapse="\n")
  tab = fread(text, sep = "\t", header = F)
  colnames(tab) = c("start", "end", "no_idea_1", "pos_x", "pos_y", "no_idea_2")
  return(tab)
}