#' Draws by default learnd and return path of a quest
#' 
#' @param data Data extracted using select_data from MultiAnalysis
#' @param quest_id ID of the quest - not session id, but the order of a quest, type of the quest is then recommended
#' @param quest_order_session Session order of the quest
#' @param map_img_path path to the image that will be overwrote
#' @return graph of the learned and trial path
draw_quest_participants = function(data, quest_id, types = c("trial"), map_img_path = NULL){
  #validation
  if(is.null(map_img_path)){
    SmartPrint(c("ERROR:draw_quest_participants:MissingParameter", "TYPE:map_img_path ", "DESCRIPTION:", "Missing parameter"))
    stop()
  }
  quest_start_and_stop = NULL
  path_table = data.table()
  first = TRUE
  for(i in 1:length(data)){
    participant_name = names(data[i])
    participant_class = data[[i]]
    quest = participant_class$PublicQuestStep(quest_id, quest_types = types)
    if(is.null(quest)) next
    ls = participant_class$quest_path(quest)
    if(is.null(ls)) next
    if (first){
      quest_start_and_stop = ls[["start_stop"]]
      map_size = participant_class$map_size()
      first = FALSE
    }
    #TODO - checks for the log to be present
    quest_player_table = ls[["player_log"]]
    modified_log = add_special_path_column(quest_player_table, participant_name)
    path_table = rbindlist(list(path_table, modified_log))
  }
  make_path_image(img_location = map_img_path, position_table = path_table, map_size = map_size, special_points = quest_start_and_stop)
}