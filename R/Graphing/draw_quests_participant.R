#' Draws by default learnd and return path of a quest
#' 
#' @param quest_set provided by the UnityAnalysis
#' @param trial_sets data saved in UnityAnalysisClass
#' @param quests_id ID of the quest - not session id, but the order of a quest
#' @param img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_quests_participant = function(quest_set, trial_sets, quest_ids, img_path = NULL){
  if(is.null(img_path)){
    SmartPrint(c("ERROR:draw_quest_participant:MissingParameter", "TYPE:img_path ", "DESCRIPTION:", "Missing parameter"))
    stop()
  }
  FIRST = T
  map_size = NULL
  path_table = data.table()
  for(quest_id in quest_ids){
    quest = get_quest(quest_set, trial_sets, quest_id)
    if(is.null(quest)) next
    ls = quest_path(quest_set, trial_sets, quest)
    if(is.null(ls)) next
    # for trial and learning separately
    if (FIRST){
      map_size = get_map_size(quest_set, trial_sets, quest)
      FIRST = F
    }
    path_table = rbindlist(list(path_table, ls$player_log))
  }
  plt = make_path_image(img_location = img_path, position_table = path_table, map_size = map_size)
  return(plt)
}