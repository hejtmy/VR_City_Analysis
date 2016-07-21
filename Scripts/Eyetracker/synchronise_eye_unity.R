#' 
#' @param events eye timestamps from the eyetracker log or key L being pressed
#' @param synchro_times data froma with times of each EyetrackerSynchro event written in player_log
#' @param fixations 
#' @param quest_times are times of start and end of quest to validate the result

synchronise_eye_unity = function(events, synchro_times, quest_times, fixations){
  
  #VALIDATION
  if (nrow(events) < 9){
    SmartPrint(c("ERROR:synchronise_eye_unity:NonValidParameter", "TYPE:events", "DESC:events have ", nrow(events), "rows. Need at least 9"))
    return(NULL)
  }
  if (nrow(synchro_times) < 9){
    SmartPrint(c("ERROR:synchronise_eye_unity:NonValidParameter", "TYPE:synchro_times", "DESC:synchro_times have ", nrow(events), "rows. Need at least 9"))
    return(NULL)
  }
  
  ALLOWED_DIFFERENCE = 25; #25ms of allowed difference between eye and unity log
  events[, diff:= c(NA, diff(time))]
  #' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
  #' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
  #' Even in correct table there are two events that are off, as those are differences between two different sets administered
  #' 
  #' If somehting is amis, we need to approximate from the data, which might or might not be possible
  #' 
  quest_times[, eye_start:= as.numeric(rep(NA,.N))]
  quest_times[, eye_end := as.numeric(rep(NA,.N))]
  
  #' we laways want at least two percise measurements per set_id - then for each set Id we calculate starting and ending synchro time
  #if(nrow(events) != length(synchro_times)){
    eye_durations = shift(events$diff, 1, type = "lead") #differences between two L times in the eyetracker log
    synchro_durations = synchro_times[,.(duration_ms=diff(time)*1000), by = set_id] #time differences between two synchroTimes for each set
    for (data_set_id in unique(synchro_times[,set_id])){
      set_synchro_durations = synchro_durations[set_id == data_set_id, duration_ms]
      eye_idx = find_appropriate_match(eye_durations, set_synchro_durations, ALLOWED_DIFFERENCE)
      #if we found a match
      if(!is.null(eye_idx)){
        time_difference = events[,time][eye_idx] - synchro_times[set_id == data_set_id, time][1]
        quest_times[id_of_set == data_set_id, eye_start:= starts * 1000 + time_difference]
        quest_times[id_of_set == data_set_id, eye_end:= ends * 1000 + time_difference]
      }
    }
  #}
  fixations = fixations_add_quest_info(fixations, quest_times)
  return(fixations)
}

can_compare = function(compare_table){
  false_values = which(compare_table$close_enough == F)
  for (id in false_values){
    if(!(compare_table$close_enough[id - 1] & compare_table$close_enough[id + 1])) return(F)
  }
  return(T)
}

#' tries to find a sequency of N elements in eye_durations that correspond to the synchro durations
#' returns index of first matchin eye event
#' 
#' @param eye_durations 
#' @param set_synchro_durations 
#' @param allowed_difference 
#' 
find_appropriate_match = function(eye_durations, set_synchro_durations, allowed_difference){
  idx_table = data.table(eye_idx = numeric(), synchro_idx = numeric())
  #finds the first closest idx after the last found index
  n_eye = length(eye_durations)
  n_synchro = length(set_synchro_durations)
  #VALIDATE
  
  sum_diff_synchro = sum(set_synchro_durations)
  best_start_value = allowed_difference * n_synchro #this initialisation insures the first assignments
  best_start_idx = NULL
  #logically this should be n_synchro - 1, but we have NA value in eye duration at the end, so we iterate only n_times
  for (eye_start_id in 1:(n_eye - n_synchro)){
    sel_eye_durations = eye_durations[eye_start_id:(eye_start_id + n_synchro - 1)]
    sum_diff_eye = abs(sum_diff_synchro - sum(sel_eye_durations))
    #if sum of differences in these elements it combined lesser than allowed difference per element, we note it
    if(sum_diff_eye < allowed_difference * n_synchro){
      # but only if its better than the already best one
      if (sum_diff_eye < best_start_value) best_start_idx = eye_start_id
    }
  }
  #if we found such a match, we note it as 
  #TODO - maybe check again
  return(best_start_idx)
  
#   for(synchro_id in 1:length(set_synchro_durations)){
#     dur = set_synchro_durations[synchro_id]
#     if(is.na(dur)) next
#     #if there are any valued smalled than allowed difference
#     #I reassign becase of NA handeling
#     is_any = any(abs(eye_durations - dur) < allowed_difference)
#     if (!is.na(is_any) & is_any){
#       #we choose the least one
#       idx = which.min(abs(eye_durations - dur))
#       idx_table = rbind(idx_table, list(idx, synchro_id))
#     }
#   }
#   return(idx_table)
}

fixations_add_quest_info = function(fixations, quest_times){
  fixations[, quest_session_id := as.numeric(rep(NA, .N))]
  for(i in 1:nrow(quest_times)){
    quest = quest_times[i, ]
    fixations[start > quest$eye_start & end < quest$eye_end, quest_session_id:=quest$session_id]
  }
  fixations = merge(fixations,quest_times[,1:6, with = FALSE],by.x = "quest_session_id", by.y = "session_id", all.x = T)
  return(fixations)
}