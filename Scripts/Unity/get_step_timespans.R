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
  # it is possible to extract only one part of the quest (e.g. was only activated or only finnished)
  step_times = reshape(step_times, timevar = c("Action"), idvar = "StepID", direction = "wide")
  if(ncol(step_times) != 3){
    SmartPrint(c("WARNING:get_step_timespans", "TYPE:Step", name, "incomplete", "QUEST:", quest$name, "DESCRIPTION: Step doesn't have complete acitvation and finish", "ACTION: Skipping"))
    return(NULL)
  }
  colnames(step_times) = c("StepID", "StepActivated", "StepFinished")
  return(step_times)
}
