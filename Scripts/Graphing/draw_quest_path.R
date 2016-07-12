#function draws by default learnd and return path of a quest
#required varialbes:
# - quest_set: provided by the UnityAnalysis
# - trial_sets: data saved in UnityAnalysisClass
# - quest_id: ID of the quest - not session id, but the order of a quest
# - img_path: path to the image that will be overwrote
draw_quest_path = function(quest_set, trial_sets, quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
  special_paths = list()
  quest_start_and_Stop = NULL
  path_table = data.table()
  first = TRUE
  for(i in 1:length(types)){
    type = types[i]
    quest = quest(quest_set, trial_sets, quest_id, quest_types = type)
    if(is.null(quest)) next
    if (first){
      quest_start_and_stop = quest_start_finish_positions(quest_set, trial_sets, quest)
      map_size = map_size(quest_set, trial_sets, quest)
    }
    special_paths[[type]] = quest_timewindow(quest, include_teleport = F)
    #adds path_table to the 
    quest_path_table = player_log_quest(quest_set, trial_sets, quest, include_teleport = F)
    path_table = rbindlist(list(path_table,quest_path_table))
    first = FALSE
  }
  make_path_image(img_location = img_path, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
}