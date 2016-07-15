eyetracker_summary = function(events, fixations, quest_times){
  ALLOWED_DIFFERENCE = 250; #250ms of allowed difference between eye and unity log
  if(nrow(events) == 0){
    return(NULL)
  }
  events[, diff:= c(NA, diff(time))]
  quest_times = mutate(quest_times, duration_ms = (ends - starts)*1000)
  
  #' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
  #' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
  #' Even in correct table there are two events that are off, as those are differences between two different sets administered
  #' 
  #' If somehting is amis, we need to approximate from the data, which might or might not be possible
  #' 
  if(nrow(events) < 10){
    
  }
  compare_table = mutate(compare_table, close_enough = (abs(diff) < 250))
  if(nrow(events) != nrow(quest_times)){
    quest_durations = quest_times$duration_ms
    eye_durations = shift(events$diff, 1, type = "lead")
  }
  if(nrow(events) == nrow(quest_times)){
    #creates a comparing table - the shift function pushes NA diff to the end
    compare_table = data.table(quest_time = quest_times$duration_ms, eye_time = shift(events$diff, 1, type = "lead"))
    compare_table = mutate(compare_table, diff = quest_time - eye_time)
    #GOOD
    if(!can_compare(compare_table)){
      quest_times$eye_start = events$time
      quest_times = mutate(quest_times, eye_end = eye_start + duration_ms)
      fixations_add_quest(fixations, quest_times)
    }
  }
}

can_compare = function(compare_table){
  false_values = which(compare_table$close_enough == F)
  for (id in false_values){
    if(!(compare_table$close_enough[id - 1] & compare_table$close_enough[id + 1])) return(F)
  }
  return(T)
}

fixations_add_quest = function(fixations, quest_times){
  fixations[, quest_session_id := numeric()]
  for(i in 1:nrow(quest_times)){
    quest = quest_times[i, ]
    fixations[start > quest$eye_start & end < quest$eye_end, quest_session_id := quest$session_id]
  }
}