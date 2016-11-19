BaseUnityAnalysis = R6Class("BaseUnityAnalysis",
  inherit = BaseAnalysis,               
  #define variables
  public = list(
    trial_sets = NULL,
    quest_set = NULL,
    initialize = function(dir, id = "", session = NULL, override = F, save = T){
      self$dir = dir
      self$SetParticipant(id)
      self$SetSession(session)
      #TODO - check the data
      if(nargs() >= 3) {
        self$ReadData()
      }
    },
    valid = function(){
      return(!is.null(self$trial_sets))
    },
    ReadData = function(override = F, save = T){
      private$readDataPrivate(override, save)
    },
    #define what is valid in the current context
    SetSession = function(number = 1){
      self$session = paste("Session", number, sep="")
    },
    QuestsSummary = function(force = F){
      return(MakeQuestsSummary(self$quest_set, self$trial_sets))
    },
    pointing_summary = function(correct_angles = NULL, override = F){
      if(override || is.null(private$dt_pointing_summary)){
        dt_point = make_pointing_summary(self$quest_set, self$trial_sets, correct_angles = correct_angles)
        private$dt_pointing_summary = dt_point
      }
      return(private$dt_pointing_summary)
    },
    #takes either quest_id or session id as a parameter
    #order_session = 
    QuestSummary = function(quest_idx = NULL, quest_order_session = NULL){
      return(MakeQuestSummary(self$quest_set, self$trial_sets, quest_idx, quest_order_session))
    },
    PublicQuestStep = function(quest_idx, quest_types = NULL){
      private$questStep(quest_idx, quest_types)
    },
    # Makes a graph with a path from start to finish
    quest_path = function(quest){
      return(quest_path(self$quest_set, self$trial_sets, quest))
    },
    entire_player_log = function(){
      return(get_entire_player_log(trial_sets))
    },
    event_times = function(event_name = NULL){
      return(get_event_times(self$trial_sets, event_name))
    },
    map_size = function(quest = NULL){
      get_map_size(self$quest_set, self$trial_sets, quest)
    },
    screen_size = function(){
      get_screen_size(self$trial_sets)
    },
    quests_timewindows = function(include_teleport = T){
      return(get_quests_timewindows(quest_set = self$quest_set, trial_sets = self$trial_sets, include_teleport = include_teleport))
    },
    DrawQuestPath = function(quest_id, types = c("learn","trial"), img_path = "Maps/megamap5.png"){
      draw_quest_participant(self$quest_set, self$trial_sets, quest_id, img_path = img_path)
    }
  ),
  private = list(
    quests_summary = NULL,
    dt_pointing_summary = NULL,
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
    },
    questStep = function(quest_idx, quest_types = NULL){
      return(QuestStep(self$quest_set, self$trial_sets, quest_idx, quest_types))
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
    playerLogForQuest = function(quest = NULL, quest_order_session = NULL, include_teleport = T){
      return(PlayerLogForQuest(quest, quest_order_session, include_teleport))
    },
    getStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
      return(GetStepTime(quest, step_name, step_action, step_id))
    }
  )
)
