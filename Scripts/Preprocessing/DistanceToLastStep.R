DistanceToLastStep = function(quest_set, quest, trial_sets){
  lastPosition = LastPlayerPositionInQuest(quest_set, quest, trial_sets)
  if(is.null(lastPosition)) return(NA)
  questLastPosition =  LastQuestPosition(quest) #keeping only X and Z
  if(is.null(questLastPosition)) return(NA)
  distance = EuclidDistanceColumns(lastPosition,questLastPosition)
  if(!is.na(distance) && !is.numeric(distance)){
    return(NA)
  }
  return(distance)
}
#returns X Z position of the player at the end of the given quest
LastPlayerPositionInQuest = function(quest_set, quest, trial_sets){
  time = GetQuestEndTime(quest)
  if (is.null(time)) return(NULL)
  playerLog = WholeTrialPlayerLogForQuest(quest_set, quest, trial_sets)
  lastPosition = get_position_at_time(playerLog, time)
  return(c(lastPosition$Position.x,lastPosition$Position.z))
}
LastQuestPosition = function(quest){
  transformsIDs = quest$steps$ID[quest$steps$Transform!="NO transform"]
  if(length(transformsIDs) == 0){
    SmartPrint(c("ERROR:LastQuestPosition:NoTransforms", "Quest: ", quest$name, "DESCRIPTION:", "There is no quest steps with transforms"))
    return(NULL)
  }
  lastID = tail(transformsIDs,1)
  return(quest_step_position(quest, lastID))
}
