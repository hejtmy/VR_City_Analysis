eyetracker_summary = function(events, fixations, quest_times, unity_events){
  quest_fixations = synchronise_eye_unity(events, unity_events, quest_times, fixations)
  return(quest_fixations)
}
