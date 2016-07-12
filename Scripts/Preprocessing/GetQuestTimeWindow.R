quest_timewindow = function(quest = NULL, quest_idx = NULL, include_teleport = T){
  ls = GetQuestTimewindow(quest, quest_idx, include_teleport)
  return(ls)
}
GetQuestTimewindow = function(quest = NULL, quest_idx = NULL, include_teleport = T){
  if(is.null(quest)){
    SmartPrint(c("ERROR:getQuestTimewindow", "Quest log not reachable"))
    return(NULL)
  }
  if(include_teleport){
    start_time = quest$data$TimeFromStart[quest$data$Action == "Quest started"]
  }else{
    start_time = GetTeleportTimes(quest)$finish
  }
  end_time = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
  #if there never was end of the quest
  if (length(end_time) < 1) end_time = tail(quest$data,1)$TimeFromStart
  ls = list()
  ls[["start"]] = start_time
  ls[["finish"]] = end_time
  return(ls)
}
GetTeleportTimes = function(quest = NULL){
  if(is.null(quest)){
    SmartPrint(c("ERROR:getTeleportTimes", "Quest log not reachable"))
    return(NULL)
  } 
  ls = list()
  ls[["start"]] = GetStepTime(quest, "Teleport Player")
  ls[["finish"]] = GetStepTime(quest, "Teleport Player", "StepFinished")
  return(ls)
}
