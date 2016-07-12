#fuinction returns player log for entire session where quest took place
#required parameters are
# - either quest or quest_session_id: quest_session_id overrides quest 
# - quest: quest object extracted from the log
# - quest_session_id: session, not quest id, of wanted quest
# quest_set: quest set table from the log object
# - trial_sets: sets of data from the original object, player log will be extracted from here

trial_player_log_quest = function(quest_set, quest = NULL, trial_sets = NULL, quest_session_id = NULL){
  return(WholeTrialPlayerLogForQuest(quest_set, quest, trial_sets, quest_session_id))
}
WholeTrialPlayerLogForQuest = function(quest_set, quest = NULL, trial_sets = NULL, quest_session_id = NULL){
  if(!is.null(quest)) quest_line = filter(quest_set, name == quest$name)
  if(!is.null(quest_session_id)) quest_line = filter(quest_set, session_id == quest_session_id)
  if(nrow(quest_line) > 1){
    print("Multiple quests have the same name")
    return(NULL)
  }
  if(is.null(trial_sets)) return(NULL)
  return(trial_sets[[quest_line$id_of_set]]$player_log)
}