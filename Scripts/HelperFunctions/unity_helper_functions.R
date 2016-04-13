get_teleport_times = function(quest_idx){
  quest = quests_log[quest_idx][[1]]
  if(is.null(quest)){
    stop("Quest log not reachable")
  }
  teleport_start_times = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" && quest$data$Action =="StepActivated"]
  teleport_finish_times = quest$data$TimeFromStart[quest$data$StepType == "Teleport Player" && quest$data$Action == "StepFinished"]
  return(c(teleport_start_times,teleport_finish_times))
}