CreateDescriptiveTable = function(table, what = NULL, by = NULL){
  tab = table[,list(mean=mean((what)), sd = sd((what)), se=sd((what))/sqrt(.N)),by=list((by))]
  return(tab)
}