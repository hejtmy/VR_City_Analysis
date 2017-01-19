add_pulses_to_mri_summary = function(df, quest_set, trial_sets){
  #adds pulses
  firstPulses = integer(nrow(df))
  numberOfPulses = integer(nrow(df))
  for(i in 1:nrow(df)){
    quest = get_quest(quest_set, trial_sets, i)
    mri_summary = calculate_mri_pulses(trial_sets, quest)
    firstPulses[i] = ifelse(length(mri_summary$firstPulse)<1, NA, mri_summary$firstPulse)
    numberOfPulses[i] = ifelse(length(mri_summary$numberOfPulses)<1, NA, mri_summary$numberOfPulses)
  }
  df = mutate(df, firstPulse = firstPulses, numberOfPulses = numberOfPulses)
  return(df)
}