fill_missing_df <- function(df_to_fill, df_filler){
  miss <- which(is.na(df_to_fill), arr.ind = T)
  if(nrow(miss) > 0){
    for(i in 1:nrow(miss)){
      df_to_fill[miss[i, 1], miss[i, 2]] <- df_filler[miss[i, 2], miss[i, 2]]
    }
  }
  return(df_to_fill)
}
