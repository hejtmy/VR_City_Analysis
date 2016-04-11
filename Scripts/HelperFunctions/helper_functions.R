require(stringr)
source_folder = function(path){
  sapply(list.files(pattern="[.]R$", path=path, full.names=TRUE), source);
}

is_between = function(numbers, between_low, between_high){
  return(sapply(numbers, function(x) (x > between_low && x < between_high)))
}