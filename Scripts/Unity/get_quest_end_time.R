get_quest_end_time = function(quest){
  stepTime = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
  if(length(stepTime) > 1){ 
    SmartPrint(c("ERROR:get_quest_end_time:MultipleEnding", quest$name, "TYPE:", "There is more quest endings"))
    return(NULL)
  }
  if(length(stepTime) ==0){ 
    SmartPrint(c("ERROR:get_quest_end_time:NoEnding", quest$name, "TYPE:", "There is no quest ending"))
    return(NULL)
  }
  return(stepTime)
}