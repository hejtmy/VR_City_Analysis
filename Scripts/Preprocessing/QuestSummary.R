MakeQuestSummary = function(quest_set, trial_sets, quest_order_session = NULL, quest_id = NULL){
  ls = list()
  if (!is.null(quest_order_session)){
    quest = QuestStep(quest_set, trial_sets, quest_order_session)
    if(is.null(quest)) return(NULL)
    quest_times = get_quest_timewindow(quest, include_teleport = F) #can be null
    ls$Time = ifelse(is.null(quest_times), NA, diff(c(quest_times$start,quest_times$finish)))
    
    player_log = player_log_quest(quest_set, trial_sets, quest)
    
    #calculating sky distance from start to goal
    start_stop = quest_start_finish_positions(quest_set, trial_sets, quest)
    if(!is.null(start_stop)) ls$sky_distance = quest_sky_distance(start_stop)
    
    #calculating time and distance covered
    if(is.null(player_log)){
      ls$Distance = NA
      ls$Finished = NA
    } else {
      #needs to find the first place after the teleport
      positions = c(head(player_log,1)$cumulative_distance, tail(player_log,1)$cumulative_distance)
      ls$Distance = diff(positions)
      ls$Finished = QuestFinished(quest)
      ls$DistanceToLastStep = distance_to_last_step(quest_set, quest, trial_sets);
    }
  }
  if (!is.null(quest_id)){
    quest_types = c("learn", "trial")
    quests = QuestStep(quest_set, trial_sets, quest_id, quest_types)
    if(!length(quests) > 0) stop("no quests were found")
    #for each list member - checking if there are two
    for(type in quest_types){
      quest = quests[[type]]
      quest_order_session = GetQuestSessionId(quest)
      summary = MakeQuestSummary(quest_set, trial_sets, quest_order_session = quest_order_session)
      ls[[type]]$Time = summary$Time
      ls[[type]]$Distace = summary$Distance
      ls[[type]]$Finished = summary$Finished
      ls$DistanceToLastStep = distance_to_last_step(quest, trial_sets);
    }
  }
  return(ls)
}
GetQuestSessionId = function(quest_set, quest){
  quest_order_session = (filter(quest_set, name == quest$name) %>% select(order_session))[[1]]
  if (length(quest_order_session) > 1) stop("There are more quests with this id. Do you have correct logs in the directory?")
  return(quest_order_session)
}
###TODO - can be NULL/NA under some circumstances
QuestFinished = function(quest){
  return (ifelse(is.null(get_lasts_step_time(quest)),FALSE,TRUE))
  return(nrow(quest$data[quest$data$Action == "Quest finished",]) > 0)
}