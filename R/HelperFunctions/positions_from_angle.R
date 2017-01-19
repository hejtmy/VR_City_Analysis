positions_from_angle = function(angle){
  rad = angle_to_radian(angle)
  vector = c(sin(rad), cos(rad))
  return(vector)
}
  
angle_to_radian = function(angle){
  return(angle/180 * pi)
}
