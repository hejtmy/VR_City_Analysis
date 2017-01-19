###TODO - can be NULL/NA under some circumstances
is_quest_finished = function(quest){
  return (ifelse(is.null(get_lasts_step_time(quest)),FALSE,TRUE))
  return (nrow(quest$data[quest$data$Action == "Quest finished",]) > 0)
}