distance_to_last_step = function(quest_set, quest, trial_sets){
  lastPosition = get_last_player_position_quest(quest_set, trial_sets, quest)
  if(is.null(lastPosition)) return(NA)
  questLastPosition =  get_last_quest_position(quest) #keeping only X and Z
  if(is.null(questLastPosition)) return(NA)
  distance = EuclidDistanceColumns(lastPosition,questLastPosition)
  if(!is.na(distance) && !is.numeric(distance)){
    return(NA)
  }
  return(distance)
}