get_eye <- function(file_path){
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