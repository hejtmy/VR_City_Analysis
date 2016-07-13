#' allows to subselect specific portion of analyses
#' @param data insides of MultiParticipantAnalyses Data
#' @param name name of the experiment to substract
select_experiment = function(data, name){
  ls = list()
  for(i in 1:length(data)){
    participant_name = names(data[i])
    data_inside = data[[i]][[name]]
    if(is.null(data_inside)) {
      SmartPrint(c("WARNING:select_experiment:MissingData", "ID:", participant_name, "TYPE:", name, "DESCRIPTION:", "Missing data inside participant"))
      next
    }
    ls[[participant_name]] = data_inside
  }
  return(ls)
}