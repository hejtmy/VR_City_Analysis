quest_start_finish_positions = function(quest_set, trial_sets, quest, include_teleport = T){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  }
  ls = list()
  time_teleport_finished = quest_timewindow(quest)$finish
  ls[["start"]] = player_position_at_time(quest_set, trial_sets, quest, time_teleport_finished)
  ls[["finish"]] = LastQuestPosition(quest)
  return(ls)
}