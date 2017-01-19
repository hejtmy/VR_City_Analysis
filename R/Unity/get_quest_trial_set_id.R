get_quest_trial_set_id = function(quest_set, quest){
  if(is.null(quest_set)) return(NULL)
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(nrow(quest_line) == 0){
    return(NULL)
  }
  if(nrow(quest_line) > 1){
    return(NULL)
  }
  return(quest_line$set_id)
}