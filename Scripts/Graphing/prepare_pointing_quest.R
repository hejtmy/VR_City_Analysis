prepare_pointing_quest = function(quest_set, trial_sets, quest, choosings){
  pointing_df = pointing_accuracy(quest_set, trial_sets, quest, choosings)
  return(pointing_df)
}