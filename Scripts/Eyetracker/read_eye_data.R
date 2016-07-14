read_eye_data = function(filepath){
  text = readLines(filepath)
  eye = get_eye(filepath)
  #finds indexes that start with MSG
  events = read_eye_events(text)
  # calibrations = ReadCalibrations(lines[CAL_indexes],length(CAL_indexes))
  fixations = read_eye_fixations(text)
  return_list = list("events" = events, "fixations" = fixations)
  return(return_list)
}