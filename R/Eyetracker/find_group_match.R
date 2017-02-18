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