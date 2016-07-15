quests_timewindows = function(quest_set, trial_sets, include_teleport = T){
  df = select(quest_set, id:name)
  n_quests = nrow(quest_set)
  df_times = data.frame(starts = numeric(n_quests), ends = numeric(n_quests))
  for(i in 1:nrow(quest_set)){
    line = quest_set[i,]
    quest = get_quest(quest_set, trial_sets, line$session_id)
    times = quest_timewindow(quest, include_teleport = include_teleport)
    df_times$starts[i] = times$start
    df_times$ends[i] = times$finish
  }
  df = bind_cols(df, df_times)
  return(df)
}