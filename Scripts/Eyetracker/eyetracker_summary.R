eyetracker_summary = function(events, fixations, quest_times, synchro_times){
  quest_fixations = synchronise_eye_unity(events, synchro_times, quest_times, fixations)
  return(quest_fixations)
}
