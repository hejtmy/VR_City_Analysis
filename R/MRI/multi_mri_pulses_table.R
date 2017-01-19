multi_mri_pulses_table = function(MultiParticipantUnityAnalysis){
  analysesNames = names(MultiParticipantUnityAnalysis$Data)
  ls = list()
  for(name in analysesNames){
    MRI = MultiParticipantUnityAnalysis$Data[[name]]$MRI
    if(is.null(MRI)) next
    pulsesTable = mri_pulses_table(MRI)
    pulsesTable[, id := name]
    ls[[name]] = pulsesTable
  }
  table = rbindlist(ls)
  table = enhance_mri_table(table)
  return(table)
}