mri_pulses_table = function(MRIAnalysis){
  player_log = MRIAnalysis$trial_sets[[1]]$player_log
  pulsesTable = player_log[Input == "fMRISynchro"]
  return(pulsesTable)
}