#' Draws by default learnd and return path of a quest
#' 
#' @param quest_set provided by the UnityAnalysis
#' @param trial_sets data saved in UnityAnalysisClass
#' @param quest_id ID of the quest - not session id, but the order of a quest
#' @param img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_quest_participant = function(quest_set, trial_sets, quest_id, types = c("learn", "trial"), img_path = NULL){
  THEME = scale_colour_pander()
  if(is.null(img_path)){
    SmartPrint(c("ERROR:draw_quest_participant:MissingParameter", "TYPE:img_path ", "DESCRIPTION:", "Missing parameter"))
    stop()
  }
  ls = prepare_quest_path(quest_set, trial_sets, quest_id, types)
  if(is.null(ls)) return(NULL)
  plt = make_path_image(img_location = img_path, position_table = ls$path_table, map_size = ls$map_size, special_points = ls$start_and_stop)
  
  #this is only for buffering purposes - could be done int he add_pointing function, but would be more intensive
  choosings = get_event_times(trial_sets, "ChooseDirection")
  
  #this is TODO - make it clearer - getting too much of quest data in each function
  quest = get_quest(quest_set, trial_sets, quest_id, quest_types = "trial")
  start_stop = get_quest_start_finish_positions(quest_set, trial_sets, quest, include_teleport = F)
  
  pointing_df = prepare_pointing_quest(quest_set, trial_sets, quest, choosings)
  plt = add_pointing_arrows(plt, pointing = pointing_df, start_stop = start_stop)
  
  plt = plt + THEME
  
  return(plt)
}


