require(data.table)

GetEye <- function(file_path){
  #Starts reading the file
  con = file(file_path, 'r');
  
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    # SEARCHES FOR THE START INFORMATION
    # which eye we will record?
    if (grepl("^START", oneLine)) {
      eye <- "unknown"
      if (grepl("LEFT", oneLine)) {
        eye <- "left"
      }
      if (grepl("RIGHT", oneLine)) {
        if (eye == "left") {
          eye <- "both"
        } else {
          eye <- "right"
        }
      }
      close(con)
      return(eye)
    }
  }
  close(con)
  return("unknown")
}

ReadEvents <- function(text, n.event){
  
  msgs<- list()
  #adds everything into a list
  for (line in text){
    msg <- strsplit(line, "[\t ]")
    msgs <- c(msgs,msg)
  }
  
  output_list <- list()
  
  mouse_move_idx <- sapply(msgs, hasWord, 'MOUSE_MOVE')
  mouse_updown_idx <- (sapply(msgs, hasWord,'MOUSE_UP') | sapply(msgs,hasWord,'MOUSE_DOWN'))
  key_updown_idx <- (sapply(msgs, hasWord, 'KEY_UP') | sapply(msgs, hasWord, 'KEY_DOWN'))
  

  mouse_move_events <- data.table(matrix(unlist(msgs[mouse_move_idx]), nrow=sum(mouse_move_idx), byrow=T))
  mouse_updown_events <- data.table(matrix(unlist(msgs[mouse_updown_idx]), nrow=sum(mouse_updown_idx), byrow=T))
  key_updown_events <- data.table(matrix(unlist(msgs[key_updown_idx]), nrow=sum(key_updown_idx), byrow=T))
  
  rest_events_idx <- !(mouse_move_idx | mouse_updown_idx | key_updown_idx)
  
  #dropping the first useless column
  
  output_list  =list('mouse_move_events' = mouse_move_events, 'mouse_updown_events' = mouse_updown_events, 'key_updown_events' = key_updown_events, 'rest_events' = msgs[rest_events_idx])
  
  return(output_list)
  
  #might have empty rows
  events <- data.frame(
    event = character(n.event),
    etime = numeric(n.event),
    mtime = numeric(n.event),
    frame = numeric(n.event),
    param = numeric(n.event),
    stringsAsFactors = F)
    ne <- 0
  
    event <- "NA"; etime <- NA; mtime <- NA; frame <- NA
    param <- NA
    etime <- as.numeric(msg[2])
    log.this <- F
    current.trial <- 0;
    
    line <- gsub("SYNC", "SYNC:", line)
    
    if (grepl("TRIALID", line)) {
      event <- "TRIALID"; log.this <- T
      param <- as.numeric(msg[4])
      current.trial <- current.trial + 1
    }
    #CURRENT TRIAL - NOT SURE WHY IS THIS HERE
    if (grepl("SYNC:START", line)) {
      event <- "SYNC:START"; log.this <- T
      mtime <- as.numeric(unlist(strsplit(msg[3], ":"))[3])
      param <- current.trial
      frame <- 0
    } 
    if (grepl("SYNC:END", line)) {
      event <- "SYNC:END"; log.this <- T
      mtime <- as.numeric(unlist(strsplit(msg[3], ":"))[3])
      param <- current.trial
      #current.trial <- NULL
    }
    if (grepl("SYNC:[0-9]", line)) {
      event <- "SYNC"; log.this <- T
      tokens <- unlist(strsplit(msg[3], ":"))
      frame <- as.numeric(tokens[2])
      mtime <- as.numeric(tokens[3])
      param <- current.trial				
    }
    
    if (grepl("DRIFTCORRECT", line)) {
      # not now
    }
    # save to data frame
    if (log.this) {
      ne <- ne + 1
      events$event[ne] <- event
      events$param[ne] <- param
      events$frame[ne] <- frame
      events$etime[ne] <- etime    # ok   			
      events$mtime[ne] <- mtime/1000    # ok   			
    }
    return(events)
}

ReadCalibrations <- function(text,ncal){
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

isLength <- function(ls, len){
  return(length(ls)==len)
}

hasWord <-function(ls,word){
  #basically iterates through list and sees if at least one of the columns returns true
  return(sum(grepl(word,ls))>0)
}
