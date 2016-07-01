PlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_session_id = NULL, include_teleport = T){
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(!is.null(quest_session_id)) quest_line = filter(quest_set, session_id == quest_session_id)
  if(nrow(quest_line) > 1){
    print("Multiple quests have the same name")
    return(NULL)
  }
  if(is.null(trial_sets)) return(NULL)
  quest_times = GetQuestTimewindow(quest, include_teleport = include_teleport)
  player_log = trial_sets[[quest_line$id_of_set]]$player_log[Time > quest_times$start & Time < quest_times$finish,]
  return(player_log)
}

WholePlayerLog = function(trial_sets){
  player_log = data.table()
  for(i in 1:length(self$trial_sets)){
    pos_tab =  self$trial_sets[[i]]$player_log
    player_log = rbindlist(list(player_log, pos_tab))
  }
  return(player_log)
}