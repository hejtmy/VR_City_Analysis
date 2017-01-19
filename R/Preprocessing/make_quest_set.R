make_quest_set = function(trial_sets){
  dt = data.table(id = numeric(0), order_session = numeric(0), name = character(0), type = character(0), set_id = numeric(0), order_set = numeric(0))
  #to keep track of the number of quests
  order_session = 1
  for (n in 1:length(trial_sets)){
    quest_logs = trial_sets[[n]]$quest_logs
    num_rows = length(quest_logs)
    dt_trial = data.table(id = numeric(num_rows), 
                          order_session = numeric(num_rows), 
                          name = character(num_rows), 
                          type = character(num_rows), 
                          set_id = numeric(num_rows),
                          ordeer_set = numeric(num_rows))
    #if we pass an empty list
    if (length(quest_logs) == 0) next
    for(i in 1:length(quest_logs)){
      #needs to pass the whole thing
      quest_info = get_quest_info(quest_logs[i])
      dt_trial[i,] = list(as.numeric(quest_info$id), order_session, quest_info$name, quest_info$type, n, i)
      order_session = order_session + 1
    }
    dt = rbindlist(list(dt,dt_trial))
  }
  return(dt)
}