get_quest_timewindow = function(quest = NULL, quest_idx = NULL, include_teleport = T){
  if(is.null(quest)){
    SmartPrint(c("ERROR:get_quest_timewindow", "Quest log not reachable"))
    return(NULL)
  }
  if(include_teleport){
    start_time = quest$data$TimeFromStart[quest$data$Action == "Quest started"]
  }else{
    start_time = get_teleport_times(quest)$finish
  }
  end_time = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
  #if there never was end of the quest
  if (length(end_time) < 1) end_time = tail(quest$data,1)$TimeFromStart
  ls = list()
  ls[["start"]] = start_time
  ls[["finish"]] = end_time
  return(ls)
}

