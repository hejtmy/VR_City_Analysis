UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
    QuestsSummary = function(force = F){
      if (!force & !is.null(private$quests_summary)) return (private$quests_summary)
      df = MakeEyetrackerQuestsSummary(self$quest_set, self$trial_sets)
      private$quests_summary = df
      return(df)
    },
    DrawQuestPath = function(quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
      draw_quest_path(self$quest_set, self$trial_sets, quest_id)
    }
    ),
    private = list(
      isValid = function(){
        if (is.null(self$experiment_log)) return(FALSE)
        if (is.null(self$position_table)) return(FALSE)
      },
      setDataDirectory = function(){
        self$data_directory = paste(self$dir, self$id, "VR", self$session, sep="/")
      }
    )
)
