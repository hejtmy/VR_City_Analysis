player_log_quest = function(quest_set, trial_sets = NULL, quest = NULL, quest_order_session = NULL, include_teleport = T){
  return(PlayerLogForQuest(quest_set, quest, trial_sets, quest_order_session, include_teleport))
}

#' Extracts playuer log information only for the duration of the quest
#' 
#' @param quest_set Important because of the information about the set in which quest took place
#' 
PlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_order_session = NULL, include_teleport = T){
  if(is.null(trial_sets)) return(NULL)
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(!is.null(quest_order_session)) quest_line = filter(quest_set, order_session == quest_order_session)
  if(nrow(quest_line) > 1){
    print("Multiple quests have the same name")
    return(NULL)
  }
  if(is.null(quest)) quest = get_quest(quest_set, trial_sets, quest_order_session)
  quest_times = GetQuestTimewindow(quest, include_teleport = include_teleport)
  player_log = trial_sets[[quest_line$set_id]]$player_log[Time > quest_times$start & Time < quest_times$finish,]
  return(player_log)
}