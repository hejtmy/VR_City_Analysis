mri_pulses_table = function(MRIAnalysis){
  player_log = MRIAnalysis$trial_sets[[1]]$player_log
  if(is.null(player_log)) return(NULL)
  pulsesTable = player_log[Input == "fMRISynchro"]
  #sometimes we want to find those pulses where event begun
  return(pulsesTable)
}

