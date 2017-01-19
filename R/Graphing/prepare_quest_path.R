#' 
#'
#' @param
#' @return returns a list with start and stop fo the quest, map_size as defined by the setting and the modified path table suitable for plot
prepare_quest_path = function(quest_set, trial_sets, quest_id, types){
  path_table = data.table()
  FIRST = T
  quest_start_and_stop = NULL
  map_size = NULL
  for(i in 1:length(types)){
    type = types[i]
    quest = get_quest(quest_set, trial_sets, quest_id, quest_types = type)
    if(is.null(quest)) next
    ls = quest_path(quest_set, trial_sets, quest)
    if(is.null(ls)) next
    # for trial and learning separately
    if (FIRST){
      quest_start_and_stop = ls[["start_stop"]]
      map_size = get_map_size(quest_set, trial_sets, quest)
      FIRST = F
    }
    #TODO - checks for the log to be present
    quest_player_table = ls[["player_log"]]
    modified_log = add_special_path_column(quest_player_table, type)
    path_table = rbindlist(list(path_table, modified_log))
  }
  if(any(nrow(path_table) == 0, is.null(quest_start_and_stop), is.null(map_size))) return(NULL)
  return_ls = list(start_and_stop = quest_start_and_stop, map_size = map_size, path_table = path_table)
  return(return_ls)
}