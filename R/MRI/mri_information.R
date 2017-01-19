mri_information = function(table){
  tab = table[,list(min = min(time.difference,na.rm = T),
                    max = max(time.difference,na.rm = T), 
                    pulses = nrow(.SD)), by = id]
  return(tab)
}