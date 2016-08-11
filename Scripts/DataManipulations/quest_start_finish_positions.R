quest_start_finish_positions = function(quest_set, trial_sets, quest, include_teleport = F){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  }
  ls = list()
  time_teleport_finished = quest_timewindow(quest, include_teleport = include_teleport)$start
  ls[["start"]] = player_position_at_time(quest_set, trial_sets, quest, time_teleport_finished)
  if(is.null(ls[["start"]])) return(NULL)
  ls[["finish"]] = LastQuestPosition(quest)
  return(ls)
}