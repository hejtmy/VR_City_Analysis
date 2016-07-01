MakeQuestsSummary = function(quest_set, trial_sets){
  df = quest_set
  if(is.null(df)) return(NULL)
  trail_times = numeric(nrow(df))
  trail_distances = numeric(nrow(df))
  for(i in 1:nrow(df)){
    quest_summary = MakeQuestSummary(quest_set, trial_sets, quest_session_id = i) #possble to get NULL
    trail_times[i] = ifelse(length(quest_summary$Time) < 1, NA, quest_summary$Time)
    trail_distances[i] = ifelse(length(quest_summary$Distance) < 1, NA, quest_summary$Distance)
  }
  df = mutate(df, time = trail_times, distance = trail_distances)
  return(df)
}