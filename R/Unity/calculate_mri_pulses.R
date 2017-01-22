#calculates order and number of fMRI pulses
calculate_mri_pulses = function(trial_sets, quest){
  ls = list()
  player_log = get_entire_player_log(trial_sets)
  timeWindow = get_quest_timewindow(quest, include_teleport = T)
  #gets the index numbers of the rows in the quest time range
  #indexes are not from the entire log, but only from the fMRI sychnro line
  pulses = player_log[Input == "fMRISynchro",.I[Time > timeWindow$start & Time < timeWindow$finish]]
  ls$numberOfPulses = length(pulses)
  ls$firstPulse = pulses[1]
  return(ls)
}