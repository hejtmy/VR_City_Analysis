#' replaces eyetracker_times for quest_times
#' 
#' @param df_sync_times
#' @param fixations
eye_to_unity_times = function(fixations, df_sync_times){
  df_sync_times = mutate(df_sync_times, time_diff = (time_unity * 1000) - time_eye)
  #' - this loop might seems weird, as it always modifies all the times regardless of the set
  #' but it actually rewrites only portions of sequencial sets - rewrites 1st with 2nd and 1st and 2nd with 3rd
  fixations[, `:=`(start_unity = as.numeric(NA),
                   end_unity = as.numeric(NA))]
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i, ]
    fixations[start >  row$time_eye & end > row$time_eye, `:=`(start_unity = (start + row$time_diff)/1000,
                                                               end_unity = (end + row$time_diff)/1000)]
  }
  return(fixations)
}

