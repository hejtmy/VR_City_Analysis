UnityTrialSet <- R6Class("UnityTrialSet",
                                   
 inherit = BaseUnityAnalysis,
 #define variables
 public = list(
   #basic definitions
   experiment_log = NULL,
   scenario_log= NULL,
   player_log = NULL,
   quest_logs = NULL,
   
   initialize = function(experiment_log, player_log,scenario_log,quest_logs){
     self$experiment_log = experiment_log
     self$scenario_log = scenario_log
     self$player_log = player_log
     self$quest_logs = quest_logs
     
     #TODO - check the data
     if(nargs() >= 4) {
       self$read_data_private()
     }
   })
)