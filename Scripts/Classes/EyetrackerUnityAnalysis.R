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
