player_log_quest = function(quest_set, trial_sets = NULL, quest = NULL, quest_session_id = NULL, include_teleport = T){
  return(PlayerLogForQuest(quest_set, quest, trial_sets, quest_session_id, include_teleport))
}
PlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_session_id = NULL, include_teleport = T){
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(!is.null(quest_session_id)) quest_line = filter(quest_set, session_id == quest_session_id)
  if(nrow(quest_line) > 1){
    print("Multiple quests have the same name")
    return(NULL)
  }
  if(is.null(trial_sets)) return(NULL)
  quest_times = GetQuestTimewindow(quest, include_teleport = include_teleport)
  player_log = trial_sets[[quest_line$id_of_set]]$player_log[Time > quest_times$start & Time < quest_times$finish,]
  return(player_log)
}