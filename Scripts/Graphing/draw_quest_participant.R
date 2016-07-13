#' Draws by default learnd and return path of a quest
#' 
#' @param quest_set provided by the UnityAnalysis
#' @param trial_sets data saved in UnityAnalysisClass
#' @param quest_id ID of the quest - not session id, but the order of a quest
#' @param img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_quest_participant = function(quest_set, trial_sets, quest_id, types = c("learn","trial"), img_path = NULL){
  #validation
  if(is.null(img_path)){
    SmartPrint(c("ERROR:draw_quest_path:MissingParameter", "TYPE:img_path ", "DESCRIPTION:", "Missing parameter"))
    stop()
  }
  special_paths = list()
  quest_start_and_stop = NULL
  path_table = data.table()
  first = TRUE
  for(i in 1:length(types)){
    type = types[i]
    quest = quest(quest_set, trial_sets, quest_id, quest_types = type)
    if(is.null(quest)) next
    ls = quest_path(quest_set, trial_sets, quest)
    if(is.null(ls)) next
    if (first){
      quest_start_and_stop = ls[["start_stop"]]
      map_size = map_size(quest_set, trial_sets, quest)
    }
    special_paths[[type]] = ls[["time"]]
    #adds path_table to the 
    quest_path_table = ls[["player_log"]]
    path_table = rbindlist(list(path_table, quest_path_table))
    first = FALSE
  }
  make_path_image(img_location = img_path, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
}