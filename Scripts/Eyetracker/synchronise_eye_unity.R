#' 
#' @param events eye timestamps from the eyetracker log or key L being pressed
#' @param unity_events data froma with times of each EyetrackerSynchro event written in player_log
#' @param fixations 
#' @param quest_times are times of start and end of quest to validate the result

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
  
  ALLOWED_DIFFERENCE = 50; #25ms of allowed difference between eye and unity log

  #' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
  #' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
  #' Even in correct table there are two events that are off, as those are differences between two different sets administered
  #' 
  #' If somehting is amis, we need to approximate from the data, which might or might not be possible
  #' 
  quest_times[, eye_start:= as.numeric(rep(NA,.N))]
  quest_times[, eye_end := as.numeric(rep(NA,.N))]
  
  #' Tries Escape
  #' Should return a data frame set_id first_index
  df_sync_times = try_fit_event("ESCAPE", "Pause", eye_events, unity_events, ALLOWED_DIFFERENCE)
  if(sum(complete.cases(df_sync_times)) == max(quest_times$set_id)){
    synchronise_quest_times(quest_times, df_sync_times)
  }
  #if it fails, tries Eyetracker synchro
  
  
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

try_fit_event = function(eye_event, unity_event, eye_times, unity_times, allowed_difference){
  eye_events = copy(eye_times)
  eye_events = eye_events[type == eye_event, ]
  eye_events[, diff:= c(NA, diff(time))]
  
  eye_durations = shift(eye_events$diff, 1, type = "lead") #differences between two L times in the eyetracker log
  unity_durations = unity_times[Input == unity_event, .(Time, duration_ms = c(diff(Time) * 1000, NA)), by = set_id]
  
  n_sets = length(unique(unity_durations[, set_id]))
  df = data.frame(set_id = unique(unity_durations[, set_id]), time_eye = rep(NA, n_sets), time_unity = rep(NA, n_sets))
  
  for (data_set_id in unique(unity_durations[, set_id])){
    unity_set_durations = unity_durations[set_id == data_set_id]
    ls_idx = find_single_match(eye_durations, unity_set_durations[, duration_ms], allowed_difference)
    if(!is.null(ls_idx)){
      df[df$set_id == data_set_id, ]$time_eye = eye_events[ls_idx$eye]$time
      df[df$set_id == data_set_id, ]$time_unity = unity_set_durations[ls_idx$unity, Time]
    }
  }
  return(df)
}

find_single_match = function(eye_durations, unity_durations, allowed_difference){
  for (i in 1:length(unity_durations)){
    dur = unity_durations[i]
    if(is.na(dur)) next
    id = which(abs(eye_durations - dur) < allowed_difference)
    if (length(id) == 1){
      return(list(unity = i, eye = id))
    }
  }
  return(NULL)
}

#' tries to find a sequency of N elements in eye_durations that correspond to the synchro durations
#' returns index of first matchin eye event
#' 
#' @param eye_durations 
#' @param set_synchro_durations 
#' @param allowed_difference 
#' 
find_group_match = function(eye_durations, set_synchro_durations, allowed_difference){
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
}

synchronise_quest_times = function(quest_times, df_sync_times){
  df_sync_times = mutate(df_sync_times, time_diff = time_eye - (time_unity * 1000))
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i,]
    quest_times[set_id == row$set_id, eye_start:= starts * 1000 + row$time_diff]
    quest_times[set_id == row$set_id, eye_end:= ends * 1000 + row$time_diff]
  }
  return(quest_times)
}

fixations_add_quest_info = function(fixations, quest_times){
  fixations[, quest_order_session := as.numeric(rep(NA, .N))]
  for(i in 1:nrow(quest_times)){
    quest = quest_times[i, ]
    fixations[start > quest$eye_start & end < quest$eye_end, quest_order_session := quest$order_session]
  }
  fixations = merge(fixations, quest_times[,1:6, with = FALSE], by.x = "quest_order_session", by.y = "order_session", all.x = T)
  return(fixations)
}