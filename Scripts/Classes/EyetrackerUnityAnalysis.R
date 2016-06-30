UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
    QuestsSummary = function(force = F){
      if (!force & !is.null(private$quests_summary)) return (private$quests_summary)
      df = super$QuestsSummary()
      private$quests_summary = df
      return(df)
    },
    DrawQuestPath = function(quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
      special_paths = list()
      quest_start_and_Stop = NULL
      path_table = data.table()
      first = TRUE
      for(i in 1:length(types)){
        type = types[i]
        quest = private$questStep(quest_id, type)
        if(is.null(quest)) next
        if (first){
          quest_start_and_stop = private$getQuestStartAndFinishPositions(quest)
          map_size = private$mapSize(quest)
        }
        time_window = private$getQuestTimewindow(quest, include_teleport = F)
        special_paths[[type]] = time_window
        #adds path_table to the 
        quest_path_table = private$playerLogForQuest(quest, F)
        path_table = rbindlist(list(path_table,quest_path_table))
        first = FALSE
      }
      make_path_image(img_location = img_path, position_table = path_table, map_size = map_size,special_paths = special_paths, special_points = quest_start_and_stop)
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
