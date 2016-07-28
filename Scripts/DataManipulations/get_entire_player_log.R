get_entire_player_log = function(trial_sets){
  player_log = data.table()
  for(i in 1:length(trial_sets)){
    pos_tab =  trial_sets[[i]]$player_log
    pos_tab[, set_id := i]
    player_log = rbindlist(list(player_log, pos_tab))
  }
  return(player_log)
}