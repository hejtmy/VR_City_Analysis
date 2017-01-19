#returns X Z position of the player at the end of the given quest
get_last_player_position_quest = function(quest_set, trial_sets, quest){
  time = get_quest_end_time(quest)
  if (is.null(time)) return(NULL)
  playerLog = player_log_quest_trial(quest_set, trial_sets, quest )
  lastPosition = get_position_at_time(playerLog, time)
  return(c(lastPosition$Position.x,lastPosition$Position.z))
}