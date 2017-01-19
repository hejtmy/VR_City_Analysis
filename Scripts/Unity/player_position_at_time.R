player_position_at_time = function(quest_set, trial_sets, quest, time){
  log = player_log_quest_trial(quest_set, trial_sets, quest)
  if(is.null(log)) return(NULL)
  position = log[Time > time, .SD[1, c(Position.x, Position.z)]]
  return(position)
}

player_position_quest_start = function(quest_set, trial_sets, quest, include_teleport = F){
  
}

player_position_step = function(){
  
}