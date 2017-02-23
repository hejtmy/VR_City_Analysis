incomplete_df <- function(df){
  return(nrow(df) != sum(complete.cases(df)))
}