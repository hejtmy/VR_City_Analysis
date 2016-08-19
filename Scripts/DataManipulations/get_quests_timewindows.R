get_quests_timewindows = function(quest_set, trial_sets, include_teleport = T){
  
  dt = copy(quest_set)
  n_quests = nrow(dt)
  df_times = data.frame(starts = numeric(n_quests), ends = numeric(n_quests))
  for(i in 1:nrow(dt)){
    line = dt[i,]
    quest = get_quest(dt, trial_sets, line$order_session)
    times = quest_timewindow(quest, include_teleport = include_teleport)
    df_times$starts[i] = times$start
    df_times$ends[i] = times$finish
  }
  dt[,`:=`(starts=df_times$starts, ends=df_times$ends)]
  return(dt)
}