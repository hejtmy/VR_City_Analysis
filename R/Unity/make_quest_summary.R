make_quest_summary = function(quest_set, trial_sets, quest_order_session = NULL, quest_id = NULL){
  ls = list()
  if (!is.null(quest_order_session)){
    quest = get_quest(quest_set, trial_sets, quest_order_session)
    if(is.null(quest)) return(NULL)
    quest_times = get_quest_timewindow(quest, include_teleport = F) #can be null
    ls$Time = ifelse(is.null(quest_times), NA, diff(c(quest_times$start,quest_times$finish)))
    
    player_log = player_log_quest(quest_set, trial_sets, quest)
    
    #calculating sky distance from start to goal
    start_stop = get_quest_start_finish_positions(quest_set, trial_sets, quest)
    if(!is.null(start_stop)) ls$sky_distance = distance_between_points(start_stop$start, start_stop$finish)
    
    #calculating time and distance covered
    if(is.null(player_log)){
      ls$Distance = NA
      ls$Finished = NA
    } else {
      #needs to find the first place after the teleport
      positions = c(head(player_log,1)$cumulative_distance, tail(player_log,1)$cumulative_distance)
      ls$Distance = diff(positions)
      ls$Finished = is_quest_finished(quest)
      ls$DistanceToLastStep = distance_to_last_step(quest_set, quest, trial_sets);
    }
  }
  if (!is.null(quest_id)){
    quest_types = c("learn", "trial")
    quests = get_quest(quest_set, trial_sets, quest_id, quest_types)
    if(!length(quests) > 0) stop("no quests were found")
    #for each list member - checking if there are two
    for(type in quest_types){
      quest = quests[[type]]
      quest_order_session = quest$order_session
      summary = make_quest_summary(quest_set, trial_sets, quest_order_session = quest_order_session)
      ls[[type]]$Time = summary$Time
      ls[[type]]$Distace = summary$Distance
      ls[[type]]$Finished = summary$Finished
      ls$DistanceToLastStep = distance_to_last_step(quest, trial_sets);
    }
  }
  return(ls)
}