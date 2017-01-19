get_lasts_step_time = function(quest, stepAction = "StepFinished"){
  return(get_step_time(quest, step_action = stepAction, step_id = max(quest$steps$ID)))
}
