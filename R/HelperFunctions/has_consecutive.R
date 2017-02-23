has_consecutive <- function(unity_durations, eye_durations, allowed_diff, consecutive){
  if(consecutive == 0) return(TRUE)
  for(n in 1:consecutive){
    dur = unity_durations[n]
    if(is.na(dur)) return(FALSE)
    if(abs(eye_durations[n] - unity_durations[n]) > allowed_diff) return(FALSE)
  }
  return(TRUE)
}