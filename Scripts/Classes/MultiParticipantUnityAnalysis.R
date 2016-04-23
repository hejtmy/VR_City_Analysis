MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
                                         
 #define variables
 public = list(
    Analyses = NULL,
    initialize = function(dir, participants,session){
      Analyses = list()
      for(i in 1:length(participants)){
        participant_code = participants[i]
        analysis = UnityEyetrackerAnalysis$new(dir,participant_code,session)
        analysis$ReadData()
        self$Analyses[[participant_code]] = analysis
      }
   },
   QuestsSummary = function(force = F){
     if (!force & !is.null(private$quest_summary_tab)) return (private$quest_summary_tab)
     final = data.frame()
     for(i in 1:length(self$Analyses)){
       ana = self$Analyses[[i]]
       df = ana$QuestsSummary()
       df = mutate(df, participant_id = rep(ana$id,nrow(df)))
       final = rbindlist(list(final,df))
     }
     private$quest_summary_tab = final
     return(final)
   }
 ),
 private = list(
   quest_summary_tab = NULL
 )
)