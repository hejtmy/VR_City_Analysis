get_event_times = function(trial_sets, event_name = NULL){
  dt_events = get_entire_player_log(trial_sets)
  dt_events = dt_events[Input != "", .(Time, Input, set_id)]
  if(nrow(dt_events) == 0) return(NULL)
  if(!is.null(event_name)){
    dt_events = dt_events[Input == event_name]
  }
  return(dt_events)
}