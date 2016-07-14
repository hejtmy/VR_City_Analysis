read_eye_movements = function(text){
  DATA_indexes = grep("^[0-9]+.*$", text)
  pseudo_file = paste(text[DATA_indexes], collapse="\n")
  
  dat = fread(pseudo_file, header = F, col.names = c("Frame", "X", "Y", "Pupil", "NoIdea", "SomeDots"))
  dat[, X:= as.double(X)]
  dat[, Y:= as.double(Y)]
  return(dat)
}