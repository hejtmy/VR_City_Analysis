source_folder = function(path){
  sapply(list.files(pattern="[.]R$", path=path, full.names=TRUE), source);
}
is_between = function(numbers, between_low, between_high){
  return(sapply(numbers, function(x) (x > between_low && x < between_high)))
}
#' This is a mess. Basically there are two ways you can send parameters inside.
#' Either as two lists, in which case X-values are X vaues and Y are Y
#' Or as two points, in which case X-value is actually [x, y] point and Y-values alse [x, y]
#' 
#' Use distance between points instead

EuclidDistanceColumns = function(x_values,y_values){
  if(is.list(x_values)){
    x = c(x_values[[1]][1], y_values[[1]][1])
    y = c(x_values[[1]][2], y_values[[1]][2])
  }
  #TODO - this is rubbish-basically it depends on which fnction it calls it and what input it passes
  if (is.numeric(x_values) && is.numeric(y_values)){ 
    x = c(x_values[1], x_values[2])
    y = c(y_values[1], y_values[2])
  }
  if(is.null(x) || is.null(y)) return(NA)
  return(sqrt(sum((x-y)^2)))
}
ColumnPresent = function(names,name){
  return(name %in% names)
}
SmartPrint = function(characters){
  print(paste(characters, sep="", collapse = " "))
}