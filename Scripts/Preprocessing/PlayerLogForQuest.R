PlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_order_session = NULL, include_teleport = T){
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(!is.null(quest_order_session)) quest_line = filter(quest_set, order_session == quest_order_session)
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
  for(i in 1:length(trial_sets)){
    pos_tab =  trial_sets[[i]]$player_log
    player_log = rbindlist(list(player_log, pos_tab))
  }
  return(player_log)
}
GetPositionAtTime = function(playerLog, time){
  line = tail(playerLog[Time < time,list(Position.x,Position.y,Position.z)],1)
  if (nrow(line) > 0) return(line)
  return(NULL)
}
