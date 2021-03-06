get_quest = function(quest_set, trial_sets, quest_idx, quest_types = NULL){
  #validations
  if(is.null(quest_set)){
    SmartPrint(c("ERROR:QuestStep:MissingParameter", "TYPE:quest_set", "DESCRIPTION:", "Missing parameter"))
    return()
  }
  if(is.null(trial_sets)){
    SmartPrint(c("ERROR:QuestStep:MissingParameter", "TYPE:trial_sets", "DESCRIPTION:", "Missing parameter"))
    return()
  }
  ls = list()
  #if the length is 0, we assume that the quest_id is order_session
  if(length(quest_types) == 0){
    quest_lines = filter(quest_set, order_session %in% quest_idx)
    if(nrow(quest_lines) == 0) return(NULL)
    #foreach - shouldn this be always single line?
    for(i in 1:nrow(quest_lines)){
      quest_line = quest_lines[i,]
      if(is.null(quest_line)) stop(quest_line)
      quest = trial_sets[[quest_line$set_id]]$quest_logs[quest_line$order_set]
      if(is.null(quest)) return(NULL)
      quest[[1]]$name = select(quest_line,name)[[1]]
      quest[[1]]$order_session = select(quest_line, order_session)[[1]]
      ls = c(ls, quest)
    }
    #$removes redundant header - we can resave it
    ls = ls[[1]]
  } 
  if(length(quest_types) > 0){
    quest_lines = filter(quest_set, id == quest_idx & type %in% quest_types)
    if(!(nrow(quest_lines) > 0)) return(NULL) 
    for(i in 1:nrow(quest_lines)){
      quest_line = quest_lines[i, ]
      ls[[quest_types[i]]] = trial_sets[[quest_line$set_id]]$quest_logs[quest_line$order_set][[1]]
      ls[[quest_types[i]]]$name = select(quest_line, name)[[1]]
      ls[[quest_types[i]]]$order_session = select(quest_line, order_session)[[1]]
    }
    #if we only searched for a signle quest
    if(length(quest_types)==1) ls = ls[[1]]
  }
  return(ls)
}