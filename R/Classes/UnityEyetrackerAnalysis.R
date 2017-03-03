UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
      eyetracker = NULL,
      QuestsSummary = function(force = F){
        if (!force & !is.null(private$quests_summary)) return (private$quests_summary)
        df = make_eyetracker_quests_summary(self$quest_set, self$trial_sets)
        private$quests_summary = df
        return(df)
      },
      eye_summary = function(force = F){
        #checks eyetracker presence
        if(!is.null(self$eyetracker)){
          return(self$eyetracker$summary(force, self))
        }
      },
      eye_synchronise = function(override = T){
        self$eyetracker$synchronise(self, override)
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
