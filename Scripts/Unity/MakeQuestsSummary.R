MakeEyetrackerQuestsSummary = function(quest_set, trial_sets){
  df = MakeQuestsSummary(quest_set, trial_sets)
  return(df)
}
MakeMRIQuestsSummary = function(quest_set, trial_sets){
  df = MakeQuestsSummary(quest_set, trial_sets)
  df = AddPulsesToSummary(df, quest_set, trial_sets)
  return(df)
}

MakeQuestsSummary = function(quest_set, trial_sets){
  df = quest_set
  if(is.null(df)) return(NULL)
  trail_times = numeric(nrow(df))
  trail_distances = numeric(nrow(df))
  trail_finished = logical(nrow(df))
  trailDistancesToEnd = numeric(nrow(df))
  quest_sky_distance = numeric(nrow(df))
  for(i in 1:nrow(df)){
    quest_summary = make_quest_summary(quest_set, trial_sets, quest_order_session = i) #possble to get NULL
    trail_times[i] = ifelse(length(quest_summary$Time) < 1, NA, quest_summary$Time)
    trail_distances[i] = ifelse(length(quest_summary$Distance) < 1, NA, quest_summary$Distance)
    trail_finished[i] = quest_summary$Finished
    trailDistancesToEnd[i] = ifelse(length(quest_summary$DistanceToLastStep) < 1, NA, quest_summary$DistanceToLastStep)
    quest_sky_distance[i] = ifelse(is.null(quest_summary$sky_distance), NA, quest_summary$sky_distance)
  }
  df = mutate(df, time = trail_times, distance = trail_distances, finished = trail_finished, 
              trailDistanceToEnd = trailDistancesToEnd, skyDistance = quest_sky_distance)
  return(df)
}

AddPulsesToSummary = function(df, quest_set, trial_sets){
  #adds pulses
  firstPulses = integer(nrow(df))
  numberOfPulses = integer(nrow(df))
  for(i in 1:nrow(df)){
    quest = get_quest(quest_set, trial_sets, i)
    mri_summary = CalculateMRIPulses(trial_sets, quest)
    firstPulses[i] = ifelse(length(mri_summary$firstPulse)<1, NA, mri_summary$firstPulse)
    numberOfPulses[i] = ifelse(length(mri_summary$numberOfPulses)<1, NA, mri_summary$numberOfPulses)
  }
  df = mutate(df, firstPulse = firstPulses, numberOfPulses = numberOfPulses)
  return(df)
}

#calculates order and number of fMRI pulses
CalculateMRIPulses = function(trial_sets, quest){
  ls = list()
  player_log = get_entire_player_log(trial_sets)
  timeWindow = get_quest_timewindow(quest, include_teleport = T)
  #gets the index numbers of the rows in the quest time range
  #indexes are not from the entire log, but only from the fMRI sychnro line
  pulses = player_log[Input == "fMRISynchro",.I[Time > timeWindow$start & Time < timeWindow$finish]]
  ls$numberOfPulses = length(pulses)
  ls$firstPulse = pulses[1]
  return(ls)
}