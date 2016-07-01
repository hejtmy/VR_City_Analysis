GetStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
  if(is.null(quest)){
    print("Quest log not reachable")
    return(NULL)
  } 
  if(!is.null(step_id)) return(quest$data$TimeFromStart[quest$data$StepID == step_id & quest$data$Action == step_action])
  stepTime = quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action]
  if(length(stepTime) > 1){ 
    SmartPrint(c("ERROR:getStepTime", quest$name, "TYPE:", "There is more steps of the same parameters: ", step_action))
    return(NULL)
  }
  return(stepTime)
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