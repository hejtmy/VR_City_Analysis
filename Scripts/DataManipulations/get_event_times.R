get_event_times = function(trial_sets, event_name = NULL){
  if(is.null(trial_sets)){
    return(NULL)
  }
  dt_events = get_entire_player_log(trial_sets)
  if(is.null(dt_events)){
    return(NULL)
  }
  dt_events = dt_events[Input != "", .(Time, Input, set_id)]
  if(nrow(dt_events) == 0) return(NULL)
  if(!is.null(event_name)){
    dt_events = dt_events[Input == event_name]
  }
  return(dt_events)
}