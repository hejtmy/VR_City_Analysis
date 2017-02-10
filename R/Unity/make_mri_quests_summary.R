make_mri_quests_summary = function(quest_set, trial_sets){
  df = make_quests_summary(quest_set, trial_sets)
  df = add_pulses_to_mri_summary(df, quest_set, trial_sets)
  return(df)
}
