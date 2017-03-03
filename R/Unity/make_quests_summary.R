make_quests_summary = function(quest_set, trial_sets){
  df = quest_set
  if(is.null(df)) return(NULL)
  trail_times = numeric(nrow(df))
  trail_distances = numeric(nrow(df))
  trail_finished = logical(nrow(df))
  trailDistancesToEnd = numeric(nrow(df))
  quest_sky_distance = numeric(nrow(df))
  quest_deliberation_stops = numeric(nrow(df))
  for(i in 1:nrow(df)){
    quest_summary = make_quest_summary(quest_set, trial_sets, quest_order_session = i) #possble to get NULL
    trail_times[i] = ifelse(length(quest_summary$Time) < 1, NA, quest_summary$Time)
    trail_distances[i] = ifelse(length(quest_summary$Distance) < 1, NA, quest_summary$Distance)
    trail_finished[i] = quest_summary$Finished
    trailDistancesToEnd[i] = ifelse(length(quest_summary$DistanceToLastStep) < 1, NA, quest_summary$DistanceToLastStep)
    quest_sky_distance[i] = ifelse(is.null(quest_summary$sky_distance), NA, quest_summary$sky_distance)
    quest_deliberation_stops[i] = ifelse(is.null(quest_summary$nDeliberationStops), NA, quest_summary$nDeliberationStops)
  }
  df = mutate(df, time = trail_times, distance = trail_distances, finished = trail_finished, 
              trailDistanceToEnd = trailDistancesToEnd, skyDistance = quest_sky_distance, deliberationStops = quest_deliberation_stops)
  return(df)
}