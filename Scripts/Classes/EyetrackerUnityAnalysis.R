data_path = "/Data"

UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
        #basic definitions
        session = NULL,
        session_dir = NULL,
    initialize = function(dir=data_path, id="", session=NULL){
       self$dir = dir
       self$SetParticipant(id)
       self$SetSession(session)
       #TODO - check the data
       if(nargs() >= 3) {
          self$ReadData()
       }
    },
    #define what is valid in the current context
    SetSession = function(number=1){
     self$session = paste("Session",number,sep="")
    },
    # Makes a graph with a path from start to finish
    MakePathImage = function(quest_session_idx = NULL, img_path = "Maps/megamap5.png"){
      #Hmakes the path for the entire thing
      if (is.null(quest_session_idx)){
        path_table = private$PlayerLog()
        map_size = private$MapSize()
        return(make_path_image(img_location = img_path, position_table = path_table, map_size = map_size))
      } else {
        quest = private$questStep(quest_session_idx)
        time_window = private$getQuestTimewindow(quest)
        if(!is.null(time_window)){
          path_table = private$selectQuestPositionData(quest,time_window)
        }
        special_paths = list()
        special_paths[["teleport"]]= private$getTeleportTimes(quest)
        quest_start_and_stop = private$getQuestStartAndFinishPositions(quest)
        map_size = private$MapSize(quest)
        if (!is.null(special_paths)){
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
        } else {
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size)
        }
      }
    },
    DrawQuestParth = function(quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
      special_paths = list()
      quest_start_and_Stop = NULL
      path_table = data.table()
      
      for(i in 1:length(types)){
        type = types[i]
        #get the session quest_idx
        quest_session_id = private$getQuestSessionId(quest_id, type)
        quest = private$questStep(quest_session_id)
        
        if (i == 1){
          quest_start_and_stop = private$getQuestStartAndFinishPositions(quest)
          map_size = private$MapSize(quest)
        }
        time_window = private$getQuestTimewindow(quest, include_teleport = F)
        special_paths[[type]] = time_window
        #adds path_table to the 
        quest_path_table = private$selectQuestPositionData(quest,time_window)
        path_table = rbindlist(list(path_table,quest_path_table))
      }
      
      make_path_image(img_location = img_path, position_table = path_table, map_size = map_size,special_paths = special_paths, special_points = quest_start_and_stop)
    }
    ),
    private = list(
      isValid = function(){
        if (is.null(self$experiment_log)) return(FALSE)
        if (is.null(self$position_table)) return(FALSE)
      },
      setSessionDirectory = function(){
        self$session_dir <- paste(self$dir,self$id,"VR",self$session,sep="/")
      },
      #uses a lot of functions from ImportinUnityLogs.R in helperFunctions
      readDataPrivate = function(override, save){
        #session folder
        private$setSessionDirectory()
        
        #open experiment_logs to see how many do we have
        experiment_logs = OpenExperimentLogs(self$session_dir)
        
        #for each experiment_log, we open player log, scenario log and appropriate quest logs
        self$trial_sets = list()
        for (i in 1:length(experiment_logs)){
          experiment_log = experiment_logs[[i]]
          player_log = OpenPlayerLog(experiment_log, override)
          #preprocesses player log
          #checks if there is everything we need and if not, recomputes the stuff
          changed = PreprocessPlayerLog(player_log)
          if (changed & save) {
            SavePreprocessedPlayer(experiment_log, player_log)
          }
          scenario_log = OpenScenarioLog(experiment_log)
          quests_logs = OpenQuestLogs(experiment_log, scenario_log)
          
          self$trial_sets[[i]] = UnityTrialSet$new(experiment_log, player_log, scenario_log, quests_logs)
        }
        self$quest_set = MakeQuestTable(self$trial_sets)
        private$isValid()
      }
    )
)
