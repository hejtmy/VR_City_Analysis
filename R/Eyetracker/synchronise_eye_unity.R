#' Returns fixations with added information about to which evnet each fixation belongs
#' 
#' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
#' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
#' Even in correct table there are two events that are off, as those are differences between two different sets administered
#' 
#' @param events eye timestamps from the eyetracker log of keys being pressed
#' @param unity_events data froma with times of each key event written in player_log
#' @param fixations to be modified and returned
#' @param quest_times are times of start and end of quest to synchronise the result

synchronise_eye_unity = function(eye_events, unity_events, quest_times, fixations){
  
  #VALIDATION
  if (nrow(eye_events) < 9){
    SmartPrint(c("ERROR:synchronise_eye_unity:NonValidParameter", "TYPE:eye_events", "DESC:eye_events have ", nrow(eye_events), "rows. Need at least 9"))
    return(NULL)
  }
  if (nrow(unity_events) < 6){
    SmartPrint(c("ERROR:synchronise_eye_unity:NonValidParameter", "TYPE:unity_events", "DESC:unity_events have ", nrow(unity_events), "rows. Need at least 9"))
    return(NULL)
  }
  
  ALLOWED_DIFFERENCE = 100; #ms of allowed difference between eye and unity log

  quest_times[, eye_start:= as.numeric(rep(NA,.N))]
  quest_times[, eye_end := as.numeric(rep(NA,.N))]
  
  #' Tries Escape
  #' Should return a data frame set_id first_index
  df_sync_times = try_fit_event("l", "EyetrackerSynchro", eye_events, unity_events, ALLOWED_DIFFERENCE, consecutive = 3)
  if (incomplete_df(df_sync_times)){
    #filling up missing values
    SmartPrint(c("WARNING:synchronise_eye_unity:NotEnoughSynchro", "TYPE:unity_events", "DESC:Using escapes to fill in data"))
    df_sync_times_extra = try_fit_event("ESCAPE", "Pause", eye_events, unity_events, ALLOWED_DIFFERENCE, consecutive = 0)
    df_sync_times = fill_missing_df(df_sync_times, df_sync_times_extra) 
  }
  synchronise_quest_times(quest_times, df_sync_times)
  #if it fails, tries Eyetracker synchro
  eye_to_unity_times(fixations, df_sync_times)
  fixations = fixations_add_quest_info(fixations, quest_times)
  return(fixations)
}