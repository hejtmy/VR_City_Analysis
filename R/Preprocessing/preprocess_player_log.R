#' This can only be done with data table as it doesn't copy itself but modifies by reference
#' it would break data.frame which by default copies itself 
#' https://stackoverflow.com/questions/11207497/r-passing-a-data-frame-by-reference
preprocess_player_log = function(position_tab){
  #check_stuff
  #check columns
  changed = F
  if (!ColumnPresent(colnames(position_tab), "Position.x")){
    position_tab = vector3_to_columns(position_tab, "Position")
    changed = T
  }
  if (!ColumnPresent(colnames(position_tab),"cumulative_distance")){
    position_tab = AddDistanceWalked (position_tab)
    changed = T
  } 
  if (changed) print("Log modified") else print("Log ok")
  return(changed)
}