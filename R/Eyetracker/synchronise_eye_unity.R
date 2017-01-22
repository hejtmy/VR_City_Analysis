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
  df_sync_times = try_fit_event("ESCAPE", "Pause", eye_events, unity_events, ALLOWED_DIFFERENCE)

  synchronise_quest_times(quest_times, df_sync_times)
  #if it fails, tries Eyetracker synchro
  eye_to_unity_times(fixations, df_sync_times)
  fixations = fixations_add_quest_info(fixations, quest_times)
  return(fixations)
}

#' Returns a data frome with synchronising times for each set
#' Table is in format of set_id, time_eyetracker, time_unity
#' @param eye_event Name of the event in the eyetracker log
#' @param unity_event Name of the event in the unity log
#' @param eye_times All eyetrackder event times
#' @param unity_times All unity event times
#' @param allowed_difference How far can two events be separated to be still considered comming from the same source in milliseconds

try_fit_event = function(eye_event, unity_event, eye_times, unity_times, allowed_difference){
  eye_events = copy(eye_times)
  eye_events = eye_events[type == eye_event, ]
  eye_events[, diff:= c(NA, diff(time))]
  
  ## NEEDS CHECKING becasue of index number
  eye_durations = shift(eye_events$diff, 1, type = "lead") #differences between two eye event times in the eyetracker log
  unity_durations = unity_times[Input == unity_event, .(Time, duration_ms = c(diff(Time) * 1000, NA)), by = set_id]
  
  set_ids = unique(unity_durations[, set_id])
  n_sets = length(set_ids) #it is possible that one proband can have only 2 or one set
  
  #preparing the data frame output
  df = data.frame(set_id = unique(unity_durations[, set_id]), time_eye = rep(NA, n_sets), time_unity = rep(NA, n_sets))
  
  for (data_set_id in set_ids){
    
    unity_set_durations = unity_durations[set_id == data_set_id]
    
    # this passes the set durations from unity and all durations from eyetracker to the function that shoudl find best match
    # find_better_match returns a list with the best synchronising event
    ls_idx = find_better_match(eye_durations, unity_set_durations[, duration_ms], allowed_difference)
    
    #if the set is accepted and it is the accepted by the defined accepting function, we fill the return df with synchronising times
    if(!is.null(ls_idx) && accepting(ls_idx)){
      df[df$set_id == data_set_id, ]$time_eye = eye_events[ls_idx$eye]$time
      df[df$set_id == data_set_id, ]$time_unity = unity_set_durations[ls_idx$unity, Time]
    }
  }
  return(df)
}

#' Returns either a list with best synchronised event or NULL if none is found
#' 
#' @param eye_durations time separation between each same eye event in milliseconds
#' @param unity_durations time separation between unity events in one set
#' @param allowed_difference allowed difference between times to be still considered similar
find_better_match = function(eye_durations, unity_durations, allowed_difference){
  n_matches = 0
  matching = list(unity = NA, eye = NA, diff =NA)
  for (i in 1:length(unity_durations)){
    dur = unity_durations[i]
    if(is.na(dur)) next
    id = which(abs(eye_durations - dur) < allowed_difference)
    if (length(id) == 1){
      n_matches = n_matches + 1
      if (is.na(matching$diff) || matching$diff < dur){
        matching$unity = i
        matching$eye = id
        matching$diff = dur
      }
    }
  }
  if (n_matches == 0){
    SmartPrint(c("WARNING:synchronise_eye_unity:NoMatch", "DESCRIPTION: No matching events found. Synchronising could not be finished"))
    return(NULL)
  } 
  matching$n_matches = n_matches
  return(matching)
}

#' This function decides whether the list is acceptable
#' 
#' @param ls list with follwing parameters diff - time duration of the event separation, n_matches - how many good matches were found
accepting = function(ls){
  MINUTE_MS = 60 * 1000
  MIN_MATCHES = 2
  if (ls$diff < MINUTE_MS) return(FALSE)
  if (ls$n_matches < MIN_MATCHES){
    SmartPrint(c("WARNING:synchronise_eye_unity:NotEnoughData", "DESCRIPTION: Synchronising only based on a single event"))
    return(TRUE)
  }
  # tell it went alright
  return(TRUE)
}

#' replaces eyetracker_times for quest_times
#' 
#' @param df_sync_times
#' @param fixations
eye_to_unity_times = function(fixations, df_sync_times){
  df_sync_times = mutate(df_sync_times, time_diff = (time_unity * 1000) - time_eye)
  #' - this loop might seems weird, as it always modifies all the times regardless of the set
  #' but it actually rewrites only portions of sequencial sets - rewrites 1st with 2nd and 1st and 2nd with 3rd
  fixations[, `:=`(start_unity = as.numeric(NA),
       end_unity = as.numeric(NA))]
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i, ]
    fixations[start >  row$time_eye & end > row$time_eye, `:=`(start_unity = (start + row$time_diff)/1000,
                                                               end_unity = (end + row$time_diff)/1000)]
  }
  return(fixations)
}


#' Returns quest_times table with added information about when those times are tied to eyetracker
#' 
#' @param quest_times
#' @param df_sync_times
synchronise_quest_times = function(quest_times, df_sync_times){
  
  if(sum(complete.cases(df_sync_times)) != max(quest_times$set_id)){
    complete_ids = df_sync_times[complete.cases(df_sync_times), "set_id"]
    SmartPrint(c("WARNING:synchronise_eye_unity:MissingSetSynchro", "DESCRIPTION: Synchronising only sets ", complete_ids))
  }
  df_sync_times = mutate(df_sync_times, time_diff = time_eye - (time_unity * 1000))
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i,]
    quest_times[set_id == row$set_id, eye_start := starts * 1000 + row$time_diff]
    quest_times[set_id == row$set_id, eye_end := ends * 1000 + row$time_diff]
  }
  return(quest_times)
}

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
