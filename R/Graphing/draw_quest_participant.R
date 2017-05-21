#' Draws by default learnd and return path of a quest
#' 
#' @param quest_set provided by the UnityAnalysis
#' @param trial_sets data saved in UnityAnalysisClass
#' @param quest_id ID of the quest - not session id, but the order of a quest
#' @param img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_quest_participant = function(quest_set, trial_sets, quest_id, types = c("learn", "trial"), img_path = NULL){
  if(is.null(img_path)){
    SmartPrint(c("ERROR:draw_quest_participant:MissingParameter", "TYPE:img_path ", "DESCRIPTION:", "Missing parameter"))
    stop()
  }
  ls = prepare_quest_path(quest_set, trial_sets, quest_id, types)
  if(is.null(ls)) return(NULL)
  plt = make_path_image(img_location = img_path, position_table = ls$path_table, map_size = ls$map_size)
  plt = add_points(plt, ls$start_and_stop)
  plt = draw_pointing_participant(plt, quest_set, trial_sets, quest_id)
  return(plt)
}


