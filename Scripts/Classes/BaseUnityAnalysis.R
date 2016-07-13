BaseUnityAnalysis <- R6Class("BaseUnityAnalysis",
  inherit = BaseAnalysis,               
  #define variables
  public = list(
    trial_sets = NULL,
    quest_set = NULL,
    data_directory = NULL,
    session = NULL,
    initialize = function(dir=data_path, id="", session=NULL){
      self$dir = dir
      self$SetParticipant(id)
      self$SetSession(session)
      #TODO - check the data
      if(nargs() >= 3) {
        self$ReadData()
      }
    },
    ReadData = function(override = F, save = T){
      private$readDataPrivate(override, save)
    },
    #define what is valid in the current context
    SetSession = function(number=1){
      self$session = paste("Session", number, sep="")
    },
    QuestsSummary = function(force = F){
      return(MakeQuestsSummary(self$quest_set, self$trial_sets))
    },
    #takes either quest_id or session id as a parameter
    #session_id = 
    QuestSummary = function(quest_idx = NULL, quest_session_id = NULL){
      return(MakeQuestSummary(self$quest_set, self$trial_sets, quest_idx, quest_session_id))
    },
    PublicQuestStep = function(quest_idx, quest_types = NULL){
      private$questStep(quest_idx, quest_types)
    },
    # Makes a graph with a path from start to finish
    MakePathImage = function(quest_session_id = NULL, img_path = "Maps/megamap5.png"){
      #Hmakes the path for the entire thing
      if (is.null(quest_session_id)){
        path_table = private$wholePlayerLog()
        map_size = self$map_size()
        return(make_path_image(img_location = img_path, position_table = path_table, map_size = map_size))
      } else {
        quest = private$questStep(quest_session_id)
        time_window = private$getQuestTimewindow(quest)
        if(!is.null(time_window)){
          path_table = private$selectQuestPositionData(quest,time_window)
        }
        special_paths = list()
        special_paths[["teleport"]]= private$getTeleportTimes(quest)
        quest_start_and_stop = private$getQuestStartAndFinishPositions(quest)
        map_size = self$map_size(quest)
        if (!is.null(special_paths)){
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size, special_paths = special_paths, special_points = quest_start_and_stop)
        } else {
          make_path_image(img_location = img_path, position_table = path_table, map_size = map_size)
        }
      }
    },
    quest_path = function(quest){
      return(quest_path(self$quest_set, self$trial_sets, quest))
    },
    map_size = function(quest = NULL){
      map_size(self$quest_set, self$trial_sets, quest)
    }
  ),
  private = list(
    quests_summary = NULL,
    #uses a lot of functions from ImportinUnityLogs.R in helperFunctions
    readDataPrivate = function(override, save){
      #session folder
      private$setDataDirectory()
      
      #open experiment_logs to see how many do we have
      experiment_logs = OpenExperimentLogs(self$data_directory)
      if(is.null(experiment_logs)){
        SmartPrint(c("Cannot find any experiment logs in ", self$data_directory))
        return(NULL)
      }
      #for each experiment_log, we open player log, scenario log and appropriate quest logs
      self$trial_sets = list()
      for (i in 1:length(experiment_logs)){
        experiment_log = experiment_logs[[i]]
        player_log = OpenPlayerLog(experiment_log, override)
        #preprocesses player log
        #checks if there is everything we need and if not, recomputes the stuff
        if(is.null(player_log)) next
        changed = PreprocessPlayerLog(player_log)
        if (changed & save) SavePreprocessedPlayer(experiment_log, player_log)
        scenario_log = OpenScenarioLog(experiment_log)
        quests_logs = OpenQuestLogs(experiment_log, scenario_log)
        
        self$trial_sets[[i]] = UnityTrialSet$new(experiment_log, player_log, scenario_log, quests_logs)
      }
      self$quest_set = MakeQuestTable(self$trial_sets)
      private$isValid()
    },
    questStep = function(quest_idx, quest_types = NULL){
      return(QuestStep(self$quest_set, self$trial_sets, quest_idx, quest_types = NULL))
    },      
    getQuestSessionId = function(quest){
      return(GetQuestSessionId(quest))
    },
    #returns list with start and finish fields
    #include teleport = T, the starting point is calculate from the beginning of the quest
    #include teleport = F, the starting point is calculate from the end of the first teleport
    getQuestTimewindow = function(quest = NULL, include_teleport = T){
      return(GetQuestTimewindow(quest, include_teleport))
    },
    questFinished = function(quest){
      return(nrow(quest$data[quest$data$Action == "Quest finished",]) > 0)
    },
    selectQuestPositionData = function(quest, time_window){
      if(missing(time_window)) stop("Need to specify time window")
      if (length(time_window)!=2) stop("Time window needs to have only two times inside")
      id_of_set = (filter(self$quest_set, name == quest$name) %>% select(id_of_set))[[1]]
      position_table = self$trial_sets[[id_of_set]]$player_log
      return(position_table[Time > time_window$start & Time < time_window$finish])
    },
    getTeleportTimes = function(quest = NULL, quest_idx=NULL){
      return(GetQuestTimewindow(quest))
    },
    getQuestStartAndFinishPositions = function(quest, include_teleport = T){
      return(quest_start_finish_positions(self$quest_set, self$trial_sets, quest, include_teleport))
    },
    wholePlayerLog = function(){
      return(wholePlayerLog(trial_sets))
    },
    playerLogForQuest = function(quest = NULL, quest_session_id = NULL, include_teleport = T){
      return(PlayerLogForQuest(quest, quest_session_id, include_teleport))
    },
    getStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
      return(GetStepTime(quest, step_name, step_action, step_id))
    }
  )
)
