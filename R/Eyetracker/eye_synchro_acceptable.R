#' This function decides whether the list is acceptable
#' 
#' @param ls list with follwing parameters diff - time duration of the event separation, n_matches - how many good matches were found
eye_synchro_acceptable = function(ls){
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