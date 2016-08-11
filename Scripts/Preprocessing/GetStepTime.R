GetStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  } 
  if(!is.null(step_id)){
    stepTime = quest$data$TimeFromStart[quest$data$StepID == step_id & quest$data$Action == step_action]
  } else {
    stepTime = quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action]
  }
  if(length(stepTime) > 1){ 
    SmartPrint(c("ERROR:getStepTime", quest$name, "TYPE:", "There is more steps of the same parameters: ", step_action))
    return(NULL)
  }
  if(length(stepTime) == 0) return(NULL)
  return(stepTime)
}
get_steps_times = function(quest, name, action = "StepActivated"){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  }
  step_times = quest$data[quest$data$StepType == name & quest$data$Action == step_action, c("StepID", "TimeFromStart")]
  return(step_times)
}

#' returns data frame with all occurances of certain step type in a form of StepID, StepActivated, StepFinished
#' 
#' @param quest Loaded quest list as by get_quest
#' @param name Name of the step, ex. "Point in Direction"
get_step_timespans = function(quest, name){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  }
  step_times = quest$data[quest$data$StepType == name, c("TimeFromStart", "Action", "StepID")]
  if(nrow(step_times) < 1 ) return(NULL)
  step_times = reshape(step_times, timevar = c("Action"), idvar = "StepID", direction = "wide")
  colnames(step_times) = c("StepID", "StepActivated", "StepFinished")
  return(step_times)
}

GetLastStepTime = function(quest, stepAction = "StepFinished"){
  return(GetStepTime(quest, step_action = stepAction, step_id = max(quest$steps$ID)))
}

GetQuestEndTime = function(quest){
  stepTime = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
  if(length(stepTime) > 1){ 
    SmartPrint(c("ERROR:GetQuestEndTime:MultipleEnding", quest$name, "TYPE:", "There is more quest endings"))
    return(NULL)
  }
  if(length(stepTime) ==0){ 
    SmartPrint(c("ERROR:GetQuestEndTime:NoEnding", quest$name, "TYPE:", "There is no quest ending"))
    return(NULL)
  }
  return(stepTime)
}