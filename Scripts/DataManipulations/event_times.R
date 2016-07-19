event_times = function(trial_sets, event_name){
  event_t = data.table(time = numeric(), set_id = integer())
  for(i in 1:length(trial_sets)){
    times = trial_sets[[i]]$player_log[Input == event_name, Time]
    if(length(times) == 0) next
    events = data.table(time = times, set_id = i)
    event_t = rbind(event_t, events)
  }
  if(nrow(event_t) == 0) return(NULL)
  return(event_t)
}