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
  lastPosition = GetPositionAtTime(playerLog, time)
  return(c(lastPosition$Position.x,lastPosition$Position.z))
}
LastQuestPosition = function(quest){
  transforms = quest$steps$Transform[quest$steps$Transform!="NO transform"]
  lastTransform = tail(transforms,1)
  return(text_to_vector3(lastTransform)[c(1,3)])
}
