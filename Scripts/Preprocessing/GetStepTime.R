GetStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  } 
  if(!is.null(step_id)) return(quest$data$TimeFromStart[quest$data$StepID == step_id & quest$data$Action == step_action])
  steps = quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action]
  if(length(steps) > 1){ 
    SmartPrint(c("ERROR:getStepTime", quest$name, "TYPE:", "There is more steps of the same parameters: ", step_action))
    return(NULL)
  }
  return(steps)
}