save_preprocessed_player = function(experiment_log, pos_tab){
  SavePreprocessedPlayer(experiment_log, pos_tab)
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