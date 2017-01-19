get_quest_session_id = function(quest_set, quest){
  quest_order_session = (filter(quest_set, name == quest$name) %>% select(order_session))[[1]]
  if (length(quest_order_session) > 1) stop("There are more quests with this id. Do you have correct logs in the directory?")
  return(quest_order_session)
}