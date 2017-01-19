read_eye_calibrations <- function(text,ncal){
  #will produce empty lines in the calibration
  calibrations <- data.frame(
    calib.time  = numeric(n.calibrations),
    trial       = numeric(n.calibrations),
    eye         = character(n.calibrations),
    rating      = character(n.calibrations),
    error.avg   = numeric(n.calibrations),
    error.max   = numeric(n.calibrations),
    stringsAsFactors = F)
  ncal <- 0
  for (line in text)
    if (grepl("!CAL VALIDATION", line) & 
        !grepl("ABORTED", line)) {
      msg <- unlist(strsplit(line, "[\t ]"))
      ncal <- ncal + 1
      v.eye    <- msg[7]
      v.rating <- msg[8]
      v.error.avg <- as.numeric(msg[10])
      v.error.max <- as.numeric(msg[12])
      calibrations$calib.time[ncal]  <- etime
      calibrations$trial[ncal]  <- current.trial
      calibrations$eye[ncal]    <- v.eye
      calibrations$rating[ncal] <- v.rating
      calibrations$error.avg[ncal] <- v.error.avg
      calibrations$error.max[ncal] <- v.error.max
    }
  return(calibrations)
}