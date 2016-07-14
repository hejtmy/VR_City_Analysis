require(data.table)



isLength <- function(ls, len){
  return(length(ls)==len)
}

hasWord <-function(ls,word){
  #basically iterates through list and sees if at least one of the columns returns true
  return(sum(grepl(word,ls))>0)
}
