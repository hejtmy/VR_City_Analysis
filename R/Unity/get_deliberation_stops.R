get_deliberation_stops <- function(player_log, deliberation_time = 3, tolerance = 0, remove_start = T){
  df = data.frame(start_time = numeric(0), length = numeric(0), cum_rotation = numeric(0))
  # we gonna assume that the log is quite orderly and that it recorded every n-ms
  #from each point we calculate the expected point 
  pl <- copy(player_log)
  pl[, time_diff := c(0, diff(Time))]
  pl[, dist_id:= rleid(distance)] #creates an id for rows with consecutive values
  #☻removing non-0 parts
  pl = pl[distance <= tolerance]
  pl[, time_delib := sum(time_diff), by = dist_id]
  if(remove_start){pl = pl[dist_id > 2]}
  pl = pl[time_delib > deliberation_time, .SD[1], by = dist_id]
  pl[, c('dist_id', 'time_diff') := NULL]
  return(pl)
}