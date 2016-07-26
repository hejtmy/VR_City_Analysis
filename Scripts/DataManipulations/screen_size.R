screen_size = function(trial_sets){
  screen = trial_sets[[1]]$experiment_log$screen
  return(list(width = as.numeric(screen$Screen_width), height = as.numeric(screen$Screen_height)))
}