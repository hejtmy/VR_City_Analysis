UnityMRIAnalysis <- R6Class("UnityMRIAnalysis",
  inherit = BaseUnityAnalysis,
  #define variables
  public = list(
   QuestsSummary = function(force = F){
     if (!force & !is.null(private$quests_summary)) return (private$quests_summary)
     df = MakeMRIQuestsSummary(self$quest_set, self$trial_set)
     private$quests_summary = df
     return(df)
   }
  ),
  private = list(
    isValid = function(){
    },
    setDataDirectory = function(){
      self$data_directory = paste(self$dir, self$id, "MRI", self$session, sep="/")
    }
  )
)
