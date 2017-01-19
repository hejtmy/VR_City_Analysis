# returns information about quest so that it can be drawn
# returns start and stop times, positions and player log for the duration
quest_path = function(quest_set, trial_sets, quest = NULL, quest_order_session = NULL){
  ls = list()
  if(is.null(quest)){
    if(is.null(quest_order_session)) {
      SmartPrint(c("ERROR:quest_path:MissingParameter", "TYPE:quest_order_session ", "DESCRIPTION:", "Missing parameters"))
      return()
    }
    quest = get_quest(quest_set, trial_sets, quest_order_session)
    if(is.null(quest_order_session)) {
      SmartPrint(c("ERROR:quest_path:NoQUest", "ID: ", quest_order_session, "DESCRIPTION:", "There is no quest of such id in the quest set"))
      return()
    }
  }
  ls[["start_stop"]] = quest_start_finish_positions(quest_set, trial_sets, quest)
  ls[["time"]] = get_quest_timewindow(quest, include_teleport = F)
  ls[["player_log"]] = player_log_quest(quest_set, trial_sets, quest, include_teleport = F)
  return(ls)
}