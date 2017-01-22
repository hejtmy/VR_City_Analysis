#' Extracts inforamtion about player pointing 
#' 
#' @param quest_set quest set as defined and preprocessed in BaseUnityAnalysis
#' @param trial_sets all trial sets holding information about particular quests and player positions
#' 
#' id  | order_session| name  | type  | set_id | order_set  | target_distance | target_angle | chosen_angle  | decision_time
#' -------------------------------------------------------------------------------------------------------------------------
#' 
make_pointing_summary = function(quest_set, trial_sets, correct_angles = NULL){
  
  if(is.null(quest_set)) return(NULL)
  target_angle = numeric(nrow(quest_set))
  chosen_angle = numeric(nrow(quest_set))
  decision_time = numeric(nrow(quest_set))
  
  # extract times from quests whenre pointing is required
  dt = data.table()
  choosings = get_event_times(trial_sets, "ChooseDirection")
  # VALIDATION
  if(is.null(choosings)){
    #report no choose directions were found
    SmartPrint(c("ERROR:make_pointing_summary:MissingEvent", "TYPE:ChooseDirection", "DESCRIPTION: No choose direction events were found"))
    return(NULL)
  }
  for(quest_order_session in quest_set$order_session){
    quest = get_quest(quest_set, trial_sets, quest_order_session)
    
    pointing_times = get_step_timespans(quest, "Point in Direction")
    if (is.null(pointing_times)) next #skipping trials without pointing
    if (nrow(pointing_times) > 2){
      #print there is an error
      SmartPrint(c("WARNING:make_pointing_summary:TooManyPoints", "QUEST:", quest$name, "ACTION:Skipping", "DESCRIPTION: Quest has", nrow(pointing_times), "pointing steps"))
      next
    }
    if (!is.null(correct_angles)){
      correct_angle = correct_angles %>% 
        filter(name == quest$name) %>% 
        select(target_angle)
      correct_angle = if(nrow(correct_angle) == 1){correct_angle$target_angle} else {NULL}
    } else {correct_angle = NULL}
    quest_pointing = pointing_accuracy(quest_set, trial_sets, quest, choosings, correct_angle) #possble to get NAS in the data frame
    quest_pointing = quest_pointing %>% mutate(quest_order_session = quest_order_session)
    #adds info about the quest
    dt = rbindlist(list(dt, quest_pointing), fill =T)
  }
  return_dt = merge(dt, quest_set, by.x = "quest_order_session", by.y = "order_session")
  return(return_dt)
}