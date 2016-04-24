source_folder = function(path){
  sapply(list.files(pattern="[.]R$", path=path, full.names=TRUE), source);
}

is_between = function(numbers, between_low, between_high){
  return(sapply(numbers, function(x) (x > between_low && x < between_high)))
}

EuclidDistanceColumns = function(x_values,y_values){
  x = c(x_values[[1]][1],y_values[[1]][1])
  y = c(x_values[[1]][2],y_values[[1]][2])
  return(sqrt(sum((x-y)^2)))
}

ColumnPresent = function(names,name){
  return(name %in% names)
}

SmartPrint = function(characters){
  print(paste(characters, sep="", collapse = " "))
}