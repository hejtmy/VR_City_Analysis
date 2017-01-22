get_teleport_times = function(quest = NULL){
  if(is.null(quest)){
    SmartPrint(c("ERROR:get_teleport_times", "Quest log not reachable"))
    return(NULL)
  } 
  ls = list()
  ls[["start"]] = get_step_time(quest, "Teleport Player")
  ls[["finish"]] = get_step_time(quest, "Teleport Player", "StepFinished")
  return(ls)
}