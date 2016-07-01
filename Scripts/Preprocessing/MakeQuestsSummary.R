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
  for(i in 1:nrow(df)){
    quest_summary = MakeQuestSummary(quest_set, trial_sets, quest_session_id = i) #possble to get NULL
    trail_times[i] = ifelse(length(quest_summary$Time) < 1, NA, quest_summary$Time)
    trail_distances[i] = ifelse(length(quest_summary$Distance) < 1, NA, quest_summary$Distance)
  }
  df = mutate(df, time = trail_times, distance = trail_distances)
  return(df)
}

AddPulsesToSummary = function(df, quest_set, trial_sets){
  #adds pulses
  firstPulses = integer(nrow(df))
  numberOfPulses = integer(nrow(df))
  for(i in 1:nrow(df)){
    quest = QuestStep(quest_set, trial_sets, i)
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
  player_log = WholePlayerLog(trial_sets)
  timeWindow = GetQuestTimewindow(quest, include_teleport = T)
  #gets the index numbers of the rows in the quest time range
  #indexes are not from the entire log, but only from the fMRI sychnro line
  pulses = player_log[Input == "fMRISynchro",.I[Time > timeWindow$start & Time < timeWindow$finish]]
  ls$numberOfPulses = length(pulses)
  ls$firstPulse = pulses[1]
  return(ls)
}