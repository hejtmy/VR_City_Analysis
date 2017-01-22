#' Adds columns to fixations table - needs to run add area first
#' 
#' @param dt_fixations fixations data table - needs to be synced and have area build up

make_fixations_summary = function(dt_fixations){
  # validate all columns are present
  dt = dt_fixations[, .(n_fix_area = .N) ,by = .(area, participant_id, name, type, id, quest_order_session)]
  
  setnames(dt, c("area", "participant_id", "quest_name", "quest_type", "quest_id", "quest_order_session", "n_fix_area"))
  
  dt[, n_fix_quest := sum(n_fix_area), by = .(participant_id, quest_name)]
  dt[, ratio:= n_fix_area/n_fix_quest]
  
  dt = dt[order(participant_id, quest_id, quest_type, area)]
  
  return(dt)
}