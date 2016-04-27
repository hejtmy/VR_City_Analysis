MRIPulsesTable = function(MRIAnalysis){
  player_log = MRIAnalysis$trial_sets[[1]]$player_log
  pulsesTable = player_log[Input == "fMRISynchro"]
  return(pulsesTable)
}
MultiMRIPulsesTable=function(MultiParticipantUnityAnalysis){
  analysesNames = names(MultiParticipantUnityAnalysis$Data)
  ls = list()
  for(name in analysesNames){
    MRI = MultiParticipantUnityAnalysis$Data[[name]]$MRI
    if(is.null(MRI)) next
    pulsesTable = MRIPulsesTable(MRI)
    pulsesTable[,id:=name]
    ls[[name]] = pulsesTable
  }
  table = rbindlist(ls)
  return(table)
}
EnhanceMRITable = function(MRIPulsesTable){
  MRIPulsesTable[,time.difference:=c(NA,diff(Time)),by=id]
  return(MRIPulsesTable)
}
MRIInformation = function(MultiParticipantUnityAnalysis){
  table=MultiMRIPulsesTable(MultiParticipantUnityAnalysis)
  table = EnhanceMRITable(table)
  table[,list(min = min(time.difference,na.rm = T),max = max(time.difference,na.rm = T), pulses = nrow(.SD)),by=id]
}