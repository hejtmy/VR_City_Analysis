MakeQuestSummary = function(quest_set, trial_sets, quest_session_id = NULL, quest_id = NULL){
  ls = list()
  if (!is.null(quest_session_id)){
    quest = QuestStep(quest_set, trial_sets, quest_session_id)
    if(is.null(quest)) return(NULL)
    quest_times = GetQuestTimewindow(quest, include_teleport = F) #can be null
    ls$Time = ifelse(is.null(quest_times), NA, diff(c(quest_times$start,quest_times$finish)))
    
    player_log = PlayerLogForQuest(quest_set, quest, trial_sets)
    if(is.null(player_log)){
      ls$Distance = NA
      ls$Finished = NA
    } else {
      #needs to find the first place after the teleport
      positions = c(head(player_log,1)$cumulative_distance, tail(player_log,1)$cumulative_distance)
      ls$Distance = diff(positions)
      ls$Finished = QuestFinished(quest)
      ls$DistanceToLastStep = DistanceToLastStep(quest_set, quest, trial_sets);
    }
  }
  if (!is.null(quest_id)){
    quest_types = c("learn","trial")
    quests = QuestStep(quest_set, trial_sets, quest_id, quest_types)
    if(!length(quests) > 0) stop("no quests were found")
    #for each list member - checking if there are two
    for(type in quest_types){
      quest = quests[[type]]
      quest_session_id = GetQuestSessionId(quest)
      summary = MakeQuestSummary(quest_set, trial_sets, quest_session_id = quest_session_id)
      ls[[type]]$Time = summary$Time
      ls[[type]]$Distace = summary$Distance
      ls[[type]]$Finished = summary$Finished
      ls$DistanceToLastStep = DistanceToLastStep(quest, trial_sets);
    }
  }
  return(ls)
}
GetQuestSessionId = function(quest){
  quest_session_id = (filter(self$quest_set,name == quest$name) %>% select(session_id))[[1]]
  if (length(quest_session_id) > 1) stop("There are more quests with this id. Do you have correct logs in the directory?")
  return(quest_session_id)
}
###TODO - can be NULL/NA under some circumstances
QuestFinished = function(quest){
  return(nrow(quest$data[quest$data$Action == "Quest finished",]) > 0)
}