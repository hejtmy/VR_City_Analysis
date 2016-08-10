#' Extracts inforamtion about player pointing 
#' 
#' @param quest_set quest set as defined and preprocessed in BaseUnityAnalysis
#' @param trial_sets all trial sets holding information about particular quests and player positions
make_pointing_summary = function(quest_set, trial_sets){
  # extract times from quests whenre pointing is required
  df = quest_set
  if(is.null(df)) return(NULL)
  target_angle = numeric(nrow(df))
  chosen_angle = numeric(nrow(df))
  decision_time = numeric(nrow(df))
  for(quest_order_session in df$order_session){
    quest = get_quest(quest_set, trial_sets, quest_order_session)
    accuracy = pointing_accuracy(quest_set, trial_sets, quest) #possble to get NULL
    speed = pointing_speed(quest_set, quest_order_session)
    #trail_times[i] = ifelse(length(quest_summary$Time) < 1, NA, quest_summary$Time)
    #trail_distances[i] = ifelse(length(quest_summary$Distance) < 1, NA, quest_summary$Distance)
    #trail_finished[i] = quest_summary$Finished
    #trailDistancesToEnd[i] = ifelse(length(quest_summary$DistanceToLastStep) < 1, NA, quest_summary$DistanceToLastStep)
  }
  df = mutate(df, target_angle = target_angle, chosen_angle = chosen_angle, decision_time = decision_time)
  return(df)
  # extract closest event after this time and before the next phase starts
  # extract poistion of the player and his rotation
  # extract position of the point at which to point
  # return table
}

#' id  | order_session| name  | type  | set_id | order_set  | target_angle | chosen_angle  | decision_time
#' -------------------------------------------------------------------------------------------------------
#' 

pointing_accuracy = function(quest_set, trial_sets, quest){
  player_log = player_log_quest(quest_set, trial_sets, quest = quest)
  start_stop = quest_start_finish_positions(quest_set, trial_sets, quest)
}

pointing_speed = function(quest){
}