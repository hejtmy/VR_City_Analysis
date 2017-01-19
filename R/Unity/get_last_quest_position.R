get_last_quest_position = function(quest){
  transformsIDs = quest$steps$ID[quest$steps$Transform!="NO transform"]
  if(length(transformsIDs) == 0){
    SmartPrint(c("ERROR:LastQuestPosition:NoTransforms", "Quest: ", quest$name, "DESCRIPTION:", "There is no quest steps with transforms"))
    return(NULL)
  }
  lastID = tail(transformsIDs, 1)
  return(quest_step_position(quest, lastID))
}
