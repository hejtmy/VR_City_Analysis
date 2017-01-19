enhance_mri_table = function(MRIPulsesTable){
  MRIPulsesTable[, time.difference := c(NA,diff(Time)), by=id]
  MRIPulsesTable[, helper := 1]
  MRIPulsesTable[, pulses.order := cumsum(helper), by=id]
  MRIPulsesTable[, helper := NULL]
  return(MRIPulsesTable)
}