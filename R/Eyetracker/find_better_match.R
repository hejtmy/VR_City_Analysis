#' Returns either a list with best synchronised event or NULL if none is found
#' 
#' @param eye_durations time separation between each same eye event in milliseconds
#' @param unity_durations time separation between unity events in one set
#' @param allowed_difference allowed difference between times to be still considered similar
find_better_match = function(eye_durations, unity_durations, allowed_difference, consecutive){
  matching = list(unity = NA, eye = NA, diff = NA)
  n_matches = 0
  for (i in 1:length(unity_durations)){
    dur = unity_durations[i]
    if(is.na(dur)) next
    id = which(abs(eye_durations - dur) < allowed_difference)
    if (length(id) == 1){
      #We try to figure out, if the consecutive synchro points also match
      if (has_consecutive(unity_durations[i:length(unity_durations)], eye_durations[id:length(eye_durations)], allowed_difference, consecutive)){
        n_matches = consecutive + 1
        matching$unity = i
        matching$eye = id
        matching$diff = dur
        break
      }
    }
  }
  if (n_matches == 0){
    SmartPrint(c("WARNING:find_better_match:NoMatch", "DESCRIPTION: No matching events found. Synchronising could not be finished"))
    return(NULL)
  } 
  matching$n_matches = n_matches
  return(matching)
}