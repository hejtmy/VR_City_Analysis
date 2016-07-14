read_eye_data = function(filepath){
  text = readLines(filepath)
  eye = get_eye(filepath)
  #finds indexes that start with MSG
  events = read_eye_events(text)
  # calibrations = ReadCalibrations(lines[CAL_indexes],length(CAL_indexes))
  movements = read_eye_movements(text)
  return_list = list("events" = events, "data" = movements)
  return(return_list)
}