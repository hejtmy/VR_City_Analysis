#fuinction returns player log for entire session where quest took place
#required parameters are
# - either quest or quest_order_session: quest_order_session overrides quest 
# - quest: quest object extracted from the log
# - quest_order_session: session, not quest order, of wanted quest
# quest_set: quest set table from the log object
# - trial_sets: sets of data from the original object, player log will be extracted from here

player_log_quest_trial = function(quest_set, trial_sets = NULL, quest = NULL, quest_order_session = NULL){
  return(WholeTrialPlayerLogForQuest(quest_set, quest, trial_sets, quest_order_session))
}
WholeTrialPlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_order_session = NULL){
  if(!is.null(quest)) quest_line = filter(quest_set, order_session == quest$order_session)
  if(!is.null(quest_order_session)) quest_line = filter(quest_set, order_session == quest_order_session)
  if(nrow(quest_line) > 1){
    print("player_log_quest_trial:: Multiple quests have the same name")
    return(NULL)
  }
  if(is.null(trial_sets)) return(NULL)
  return(trial_sets[[quest_line$set_id]]$player_log)
}