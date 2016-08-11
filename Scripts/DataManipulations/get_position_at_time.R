get_position_at_time = function(log, time){
  line = tail(log[Time < time,list(Position.x,Position.y,Position.z)], 1)
  if (nrow(line) > 0) return(line)
  return(NULL)
}