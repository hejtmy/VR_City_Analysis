#' adds information about event to each fixation
#' 
#' @param fixations preprocessed fixations sent from the EyetrackerAnalysis class
#' @param quest_times with added information about eyetracker timestamps from synchronise_quest_times
fixations_add_quest_info = function(fixations, quest_times){
  fixations[, quest_order_session := as.numeric(rep(NA, .N))]
  for(i in 1:nrow(quest_times)){
    quest = quest_times[i, ]
    fixations[start > quest$eye_start & end < quest$eye_end, quest_order_session := quest$order_session]
  }
  fixations = merge(fixations, quest_times[, 1:6, with = FALSE], by.x = "quest_order_session", by.y = "order_session", all.x = T)
  return(fixations)
}