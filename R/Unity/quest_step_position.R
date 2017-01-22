quest_step_position = function(quest = NULL, step_id){
  #parameter validation
  if(is.null(quest)){
    SmartPrint(c("ERROR:quest_step_position:MissingParameter", "TYPE:quest", "DESCRIPTION:", "parameter not provided"))
    return(NULL)
  }
  if(!is.numeric(step_id)){
    SmartPrint(c("ERROR:quest_step_position:WrongParameterType", "TYPE:step_id", "DESCRIPTION:", "Parameter has type ", (class(step_id))," required is numeric"))
    return(NULL)
  }
  step = quest$steps %>% filter(ID == step_id)
  if(nrow(step) == 0){
    SmartPrint(c("ERROR:quest_step_position:MissingStep", "ID: ", step_id, "DESCRIPTION:", "There is no quest of such ID"))
    return(NULL)
  } 
  if(nrow(step) == 0){
    SmartPrint(c("ERROR:quest_step_position:NonUnique", "ID: ", step_id, "DESCRIPTION:", "There are more quests with given ID"))
    return(NULL)
  } 
  if(step$Transform =="NO transform"){
    SmartPrint(c("WARNING:quest_step_position:NOTransform", "ID: ", step_id, "DESCRIPTION:", "Step has no transform"))
    return(NULL)
  }
  return(text_to_vector3(step$Transform)[c(1,3)])
}