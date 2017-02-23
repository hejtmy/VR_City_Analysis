#' Returns a data frome with synchronising times for each set
#' Table is in format of set_id, time_eyetracker, time_unity
#' @param eye_event Name of the event in the eyetracker log
#' @param unity_event Name of the event in the unity log
#' @param eye_times All eyetrackder event times
#' @param unity_times All unity event times
#' @param allowed_difference How far can two events be separated to be still considered comming from the same source in milliseconds

try_fit_event = function(eye_event, unity_event, eye_times, unity_times, allowed_difference, consecutive = 3){
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
    ls_idx = find_better_match(eye_durations, unity_set_durations[, duration_ms], allowed_difference, consecutive)
    
    #if the set is accepted and it is the accepted by the defined accepting function, we fill the return df with synchronising times
    if(!is.null(ls_idx) && eye_synchro_acceptable(ls_idx)){
      df[df$set_id == data_set_id, ]$time_eye = eye_events[ls_idx$eye]$time
      df[df$set_id == data_set_id, ]$time_unity = unity_set_durations[ls_idx$unity, Time]
    }
  }
  return(df)
}
