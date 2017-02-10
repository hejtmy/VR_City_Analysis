#' Returns quest_times table with added information about when those times are tied to eyetracker
#' 
#' @param quest_times
#' @param df_sync_times
synchronise_quest_times = function(quest_times, df_sync_times){
  
  if(sum(complete.cases(df_sync_times)) != max(quest_times$set_id)){
    complete_ids = df_sync_times[complete.cases(df_sync_times), "set_id"]
    SmartPrint(c("WARNING:synchronise_eye_unity:MissingSetSynchro", "DESCRIPTION: Synchronising only sets ", complete_ids))
  }
  df_sync_times = mutate(df_sync_times, time_diff = time_eye - (time_unity * 1000))
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i,]
    quest_times[set_id == row$set_id, eye_start := starts * 1000 + row$time_diff]
    quest_times[set_id == row$set_id, eye_end := ends * 1000 + row$time_diff]
  }
  return(quest_times)
}
