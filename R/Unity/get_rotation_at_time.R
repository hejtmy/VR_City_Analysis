get_rotation_at_time = function(quest_set, trial_sets, quest, time){
  log = player_log_quest_trial(quest_set, trial_sets, quest)
  if(is.null(log)) return(NULL)
  rotation = log[Time > time, .SD[1, c(Rotation.X)]]
  return(rotation)
}