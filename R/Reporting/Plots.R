CompareMeansPlot = function(pltTab, what = NULL ){
  plt = ggplot(pltTab, aes_string(y='mean',x=what))
  plt = plt +  geom_bar(aes_string(fill = what), stat='identity')
  plt = plt + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),size=.3,width=0.4,position=position_dodge(.9))
  return(plt)
}
