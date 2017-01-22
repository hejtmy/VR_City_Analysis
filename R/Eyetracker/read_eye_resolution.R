#goes through the asc log and finds display options
read_eye_resolution = function(path){
  con = file(path, open = "r")
  disp_resolution = NULL
  # Needs <- assign becasue it doesn't work otherwise in the length function
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    # EXAMPLE = MSG	21256557 DISPLAY_COORDS 0 0 1919 1079
    if(grepl("DISPLAY_COORDS", line)){
      #' match two digits at least three digit long after Display coords
      #' ? signifies non greedy match (as least as possible)
      ptr = ".*DISPLAY_COORDS.*?(\\d{3,})\\s*(\\d{3,})"
      disp_resolution = gsub(ptr, "\\1;\\2", line)
      sep = strsplit(disp_resolution, ";")
      width = as.numeric(sep[[1]][1])
      height = as.numeric(sep[[1]][2])
      width = ceiling(width/10)*10
      height = ceiling(height/10)*10
      disp_resolution = (list("width" = width, "height" = height))
      break
    }
  }
  close(con)
  return(disp_resolution)
}