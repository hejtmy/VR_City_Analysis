# mri_pointing_pulses
ls = list()

for (i in 1:length(Analyses$Data)){
  
  Anal = Analyses$Data[[i]]$MRI
  if(is.null(Anal)) next
  print(Anal$id)
  pulses_times = mri_pulses_table(Anal)
  if(is.null(pulses_times) || pulses_times[,.N] == 0) next
  
  pulses_times[, c(2:12):= NULL]
  pulses_times[, i := 1:.N]
  quest_time_windows = data.table(Anal$quests_timewindows())
  if(is.null(quest_time_windows)) next
  
  quest_sum = data.table(Anal$QuestsSummary())
  if(is.null(quest_sum)) next
  
  pointing_times = Anal$event_times("ChooseDirection")
  if(is.null(pointing_times)) next
  
  pointing_times[, c(2:3) := NULL]
  quest_point_times = cross_join_dt(quest_time_windows, pointing_times)
  quest_point_times = quest_point_times[Time > starts & Time < ends]
  
  pulses_point_times = cross_join_dt(pulses_times, quest_point_times)
  pulses_point_times = pulses_point_times[Time > i.Time]
  setorder(pulses_point_times, "i.Time", "Time")
  pulses_point_times = pulses_point_times[,.SD[1], by = .(i.Time)]
  pulses_point_times[, point_end_diff := Time - i.Time]
  
  # merge with mri quest
  setkey(pulses_point_times, name)
  setkey(quest_sum, name)
  new = merge(pulses_point_times, quest_sum)
  new[, point_start_pulse := firstPulse]
  new[, point_end_pulse := i]
  new[, c(2:6, 9:12,14:25) := NULL]
  report = new[, participant_id := Anal$id]
  ls[[Anal$id]] = report
}
