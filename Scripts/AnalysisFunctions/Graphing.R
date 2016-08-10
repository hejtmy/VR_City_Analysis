SaveAllUnityEyetrackerGraphs = function(MultiParticipantUnityAnalysis){
  for(i in 1:length(MultiParticipantUnityAnalysis$Data)){
    participants = names(MultiParticipantUnityAnalysis$Data)
    eyetrackerAnalysis = MultiParticipantUnityAnalysis$Data[[i]]$UnityEyetracker
    for(n in unique(eyetrackerAnalysis$quest_set$id)){
      if(is.null(MultiParticipantUnityAnalysis$Data[[i]]$UnityEyetracker)) next
      plot = eyetrackerAnalysis$DrawQuestPath(n)
      name = paste(participants[i],"_",eyetrackerAnalysis$quest_set$name[n],".png",sep="")
      SmartPrint(c("Saving ", name))
      SavePlot(plot, name)
    }
  }
}