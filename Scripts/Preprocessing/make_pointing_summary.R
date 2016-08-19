#' Extracts inforamtion about player pointing 
#' 
#' @param quest_set quest set as defined and preprocessed in BaseUnityAnalysis
#' @param trial_sets all trial sets holding information about particular quests and player positions
#' 
#' id  | order_session| name  | type  | set_id | order_set  | target_distance | target_angle | chosen_angle  | decision_time
#' -------------------------------------------------------------------------------------------------------------------------
#' 
make_pointing_summary = function(quest_set, trial_sets){
  
  if(is.null(quest_set)) return(NULL)
  target_angle = numeric(nrow(quest_set))
  chosen_angle = numeric(nrow(quest_set))
  decision_time = numeric(nrow(quest_set))
  
  # extract times from quests whenre pointing is required
  dt = data.table(quest_order_session = numeric(), target_angle = numeric(), chosen_angle = numeric(), decision_time = numeric())
  
  for(quest_order_session in quest_set$order_session){
    quest = get_quest(quest_set, trial_sets, quest_order_session)
    choosings = get_event_times(trial_sets, "ChooseDirection")
    # VALIDATIONS
    if(is.null(choosings)){
      #report no choose directions were found
      SmartPrint(c("ERROR:make_pointing_summary:MissingEvent", "TYPE:ChooseDirection", "DESCRIPTION: No choose direction events were found"))
      return(NULL)
    }
    pointing_times = get_step_timespans(quest, "Point in Direction")
    if (is.null(pointing_times)) next #skipping trials without pointing
    if (nrow(pointing_times) > 2){
      #print there is an error
      SmartPrint(c("WARNING:make_pointing_summary:TooManyPoints", "QUEST:", quest$name, "ACTION:Skipping", "DESCRIPTION: Quest has", nrow(pointing_times), "pointing steps"))
      next
    }
    
    quest_pointing = pointing_accuracy(quest_set, trial_sets, choosings, quest, pointing_times) #possble to get NAS in the data frame
    quest_pointing = quest_pointing %>% mutate(quest_order_session = quest_order_session)
    #adds info about the quest
    dt = rbindlist(list(dt, quest_pointing), fill =T)
  }
  return_dt = merge(dt, quest_set, by.x = "quest_order_session", by.y = "order_session")
  return(return_dt)
}

#' Returns small data table

pointing_accuracy = function(quest_set, trial_sets, choosings, quest, pointing_times){
  ALLOWED_DIFFERENCE = 0.1
  
  n_pointing = nrow(pointing_times)
  
  df = data.frame(pointing_order = as.numeric(rep(NA, n_pointing)), target_angle = as.numeric(rep(NA, n_pointing)), chosen_angle = as.numeric(rep(NA, n_pointing)), decision_time = as.numeric(rep(NA, n_pointing)))
  
  # assumes that pointing times are in order
  player_log = player_log_quest_trial(quest_set, trial_sets, quest = quest)
  quest_start_finish = quest_start_finish_positions(quest_set, trial_sets, quest, include_teleport = F)
  
  quest_trial_set_id = get_quest_trial_set_id(quest_set, quest)
  if (is.null(quest_trial_set_id)) return(df)
  
  #' splitting to the first and second part
  #' First shoudl be occuring on the start and second on the end
  for (i in 1:n_pointing){
    if(i == 1){target_pos = quest_start_finish$finish} else {target_pos = quest_start_finish$start}
    # time is the tiem between
    dt_time = pointing_times[i, ]
    
    # This should be more accurate than StepFinished
    player_point_time = choosings %>% filter(set_id == quest_trial_set_id) %>% 
      filter(Time > dt_time$StepActivated) %>%
      filter((Time - dt_time$StepFinished) < ALLOWED_DIFFERENCE) %>%
      select(Time) %>% first
    
    if(length(player_point_time) != 1) next
    
    pointing_moment = player_log[Time > player_point_time, .SD[1]]
    player_pos = pointing_moment[,  c(Position.x, Position.z)]

    target_angle = angle_from_positions(player_pos, target_pos)
    chosen_angle = pointing_moment$Rotation.X
    decision_time = player_point_time - dt_time$StepActivated
    df[i,] = c(i, target_angle, chosen_angle, decision_time)
  }
  return(df)
}