#' Returns small data frame 
#' Based on the quest it searches for correct place in the player log and calculates pointing direction from the player
#' position at that time adn the position of the goal of the quest as suggested from the last known transform
#' 
#' @param choosings - evet times of ChooseDirections in Unity log - data.frame
#' @param quest - as returend by get_quest
#' @return data.frame 
pointing_accuracy = function(quest_set, trial_sets, quest, choosings = NULL){
  ALLOWED_DIFFERENCE = 0.1
  
  pointing_times = get_step_timespans(quest, "Point in Direction")
  
  n_pointing = nrow(pointing_times)
  df = data.frame(pointing_order = as.numeric(rep(NA, n_pointing)), 
                  target_angle = as.numeric(rep(NA, n_pointing)), 
                  chosen_angle = as.numeric(rep(NA, n_pointing)), 
                  point_start = as.numeric(rep(NA, n_pointing)),
                  point_end = as.numeric(rep(NA, n_pointing)))
  
  # assumes that pointing times are in order
  player_log = player_log_quest_trial(quest_set, trial_sets, quest = quest)
  quest_start_finish = quest_start_finish_positions(quest_set, trial_sets, quest, include_teleport = F)
  if(is.null(choosings)) choosings = get_event_times(trial_sets, "ChooseDirection")
  
  quest_trial_set_id = get_quest_trial_set_id(quest_set, quest)
  
  if (is.null(quest_trial_set_id)) return(df)
  
  #' splitting to the first and second part
  #' First shoudl be occuring on the start and second on the end
  for (i in 1:n_pointing){
    if(i == 1){target_pos = quest_start_finish$finish} else {target_pos = quest_start_finish$start}
    # time is the tiem between
    dt_time = pointing_times[i, ]
    
    #' This should be more accurate than StepFinished - selects ChooseDirection event
    #' from the player log rather than from the quest log
    player_point_time = choosings %>% filter(set_id == quest_trial_set_id) %>% 
      filter(Time > dt_time$StepActivated) %>%
      filter((Time - dt_time$StepFinished) < ALLOWED_DIFFERENCE) %>%
      select(Time) %>% first
    
    if(length(player_point_time) != 1) next
    
    #' selecets the correct position from the player log
    pointing_moment = player_log[Time > player_point_time, .SD[1]]
    player_pos = pointing_moment[, c(Position.x, Position.z)]
    
    target_angle = angle_from_positions(player_pos, target_pos)
    chosen_angle = pointing_moment$Rotation.X
    point_start = player_point_time 
    point_end = dt_time$StepActivated
    df[i, ] = c(i, target_angle, chosen_angle, point_start, point_end)
  }
  return(df)
}