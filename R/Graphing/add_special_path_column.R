#' Adds columns ot the data table that defines certain parts to belong to certain conditions
#' @param posision_table table to be modified
#' @param name what to put into the column 
add_special_path_column = function(position_table, name){
  position_table = position_table[, special:= name]
  return(position_table)
}