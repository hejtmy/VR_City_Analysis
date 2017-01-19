distance_between_points = function(point_x, point_y){
  if (is.numeric(point_x) && is.numeric(point_y)){ 
    x = point_x
    y = point_y
  }
  if(is.null(x) || is.null(y)) return(NA)
  return(sqrt(sum((x-y)^2)))
}