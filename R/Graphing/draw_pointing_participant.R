
#' Draws by default learnd and return path of a quest
#' 
#' @param quest_set provided by the UnityAnalysis
#' @param trial_sets data saved in UnityAnalysisClass
#' @param quest_id ID of the quest - not session id, but the order of a quest
#' @param img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_pointing_participant = function(plt, quest_set, trial_sets, quest_id){
  
  #this is only for buffering purposes - could be done int he add_pointing function, but would be more intensive
  choosings = get_event_times(trial_sets, "ChooseDirection")
  
  #this is TODO - make it clearer - getting too much of quest data in each function
  quest = get_quest(quest_set, trial_sets, quest_id, quest_types = "trial")
  start_stop = get_quest_start_finish_positions(quest_set, trial_sets, quest, include_teleport = F)
  
  pointing_df = prepare_pointing_quest(quest_set, trial_sets, quest, choosings)
  plt = add_pointing_arrows(plt, pointing = pointing_df, start_stop = start_stop)

  return(plt)
}