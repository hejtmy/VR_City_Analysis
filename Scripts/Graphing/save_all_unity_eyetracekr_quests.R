save_all_unity_eyetracekr_quest_paths = function(MultiParticipantUnityAnalysis){
  FOLDER = "../Images/Eyetracker/"
  for(i in 1:length(MultiParticipantUnityAnalysis$Data)){
    participants = names(MultiParticipantUnityAnalysis$Data)
    eyetrackerAnalysis = MultiParticipantUnityAnalysis$Data[[i]]$UnityEyetracker
    if(is.null(eyetrackerAnalysis)) next
    for(n in unique(eyetrackerAnalysis$quest_set$id)){
      plot = eyetrackerAnalysis$DrawQuestPath(n)
      if(is.null(plot)) next
      quest_name = eyetrackerAnalysis$quest_set %>% 
        filter(id == n & type == 'trial') %>%
        select(name)
      if(length(quest_name) != 1) next
      name = paste(participants[i], "_", quest_name, ".png", sep = "")
      SmartPrint(c("Saving ", name))
      save_path_plot(plot, name, FOLDER)
    }
  }
}