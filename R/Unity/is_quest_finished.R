###TODO - can be NULL/NA under some circumstances
is_quest_finished = function(quest){
  if(nrow(quest$data[quest$data$Action == "Quest finished", ]) < 0){return(FALSE)}
  if(is.null(get_lasts_step_time(quest))){return(FALSE)}
  return(TRUE)
}