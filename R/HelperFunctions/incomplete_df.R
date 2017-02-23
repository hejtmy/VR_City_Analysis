incomplete_df <- function(df){
  #' this is slightly weird, but it stems from the return function of some element, 
  #' when the returned df has actually 0 rows instead of 3 empty rows
  if(nrow(df) == 0) return(TRUE)
  return(nrow(df) != sum(complete.cases(df)))
}