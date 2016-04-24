BaseUnityAnalysis <- R6Class("BaseUnityAnalysis",
  inherit = BaseAnalysis,               
  #define variables
  public = list(
    trial_sets = NULL,
    quest_set = NULL,
    data_directory = NULL,
    ReadData = function(override = F, save = T){
      private$readDataPrivate(override, save)
    },
    QuestsSummary = function(){
      df = self$quest_set
      trail_times = numeric(nrow(df))
      trail_distances = numeric(nrow(df))
      for(i in 1:nrow(df)){
        quest_summary = self$QuestSummary(quest_session_id = i)
        trail_times[i] = ifelse(length(quest_summary$Time)<1, NA, quest_summary$Time)
        trail_distances[i] = ifelse(length(quest_summary$Distance)<1, NA, quest_summary$Distance)
      }
      df = mutate(df, time = trail_times, distance = trail_distances)
      return(df)
    },
    #takes either quest_id or session id as a parameter
    QuestSummary = function(quest_idx = NULL, quest_session_id = NULL){
      ls = list()
      if (is.null(quest_idx)){
        quest = private$questStep(quest_session_id)
        quest_times = private$getQuestTimewindow(quest, include_teleport = F)
        ls$Time = diff(c(quest_times$start,quest_times$finish))
        player_log = private$playerLogForQuest(quest)
        #needs to find the first place after the teleport
        positions = c(head(player_log,1)$cumulative_distance, tail(player_log,1)$cumulative_distance)
        ls$Distance = diff(positions)
        ls$Finished = private$questFinished(quest)
      }
      if (is.null(quest_session_id)){
        quest_types = c("learn","trial")
        quests = private$questStep(quest_idx, quest_types)
        if(!length(quests) > 0) stop("no quests were found")
        #for each list member - checking if there are two
        for(type in quest_types){
          quest = quests[[type]]
          quest_session_id = private$getQuestSessionId(quest)
          summary = self$QuestSummary(quest_session_id = quest_session_id)
          ls[[type]]$Time = summary$Time
          ls[[type]]$Distace = summary$Distance
          ls[[type]]$Finished = summary$Finished
        }
      }
      return(ls)
    },
    PublicQuestStep = function(quest_idx, quest_types = NULL){
      private$questStep(quest_idx, quest_types)
    },
    # Makes a graph with a path from start to finish
    MakePathImage = function(quest_session_id = NULL, img_path = "Maps/megamap5.png"){
      #Hmakes the path for the entire thing
      if (is.null(quest_session_id)){
        path_table = private$wholePlayerLog()
        map_size = private$mapSize()
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
        map_size = private$mapSize(quest)
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
          map_size = private$mapSize(quest)
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
    #uses a lot of functions from ImportinUnityLogs.R in helperFunctions
    readDataPrivate = function(override, save){
      #session folder
      private$setDataDirectory()
      
      #open experiment_logs to see how many do we have
      experiment_logs = OpenExperimentLogs(self$data_directory)
      
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
      ls = list()
      #if the length is 0, we assume that the quest_idx is quest_session_id
      if (length(quest_types) == 0){
        quest_lines = filter(self$quest_set, session_id %in% quest_idx)
        if(nrow(quest_lines) == 0) return(NULL);
        #foreach
        for(i in 1:nrow(quest_lines)){
          quest_line = quest_lines[i]
          quest = self$trial_sets[[quest_line$id_of_set]]$quest_logs[quest_line$set_id]
          quest[[1]]$name = select(quest_line,name)[[1]]
          ls = c(ls,quest)
        }
        #$removes redundant header - we can resave it
        ls = ls[[1]]
      } 
      if(length(quest_types) > 0){
        quest_session_id  = filter(self$quest_set, id == quest_idx & type %in% quest_types) %>% select(session_id)
        quest_lines = filter(self$quest_set, session_id %in% quest_session_id[[1]])
        #foreach
        for(i in 1:nrow(quest_lines)){
          quest_line = quest_lines[i]
          ls[[quest_types[i]]] = self$trial_sets[[quest_line$id_of_set]]$quest_logs[quest_line$set_id][[1]]
          ls[[quest_types[i]]]$name = select(quest_line,name)[[1]]
        }
      }
      return(ls)
    },      
    getQuestSessionId = function(quest){
      quest_session_id = (filter(self$quest_set,name == quest$name) %>% select(session_id))[[1]]
      if (length(quest_session_id) > 1) stop("There are more quests with this id. Do you have correct logs in the directory?")
      return(quest_session_id)
    },
    getQuestTimewindow = function(quest = NULL, quest_idx = NULL, include_teleport = T){
      if(is.null(quest)) quest = private$questStep(quest_idx)
      if(is.null(quest)) stop("Quest log not reachable")
      if(include_teleport){
        start_time = quest$data$TimeFromStart[quest$data$Action == "Quest started"]
      }else{
        start_time = private$getTeleportTimes(quest)$finish
      }
      end_time = quest$data$TimeFromStart[quest$data$Action == "Quest finished"]
      #if there never was end of the quest
      if (length(end_time) < 1)end_time = tail(quest$data,1)$TimeFromStart
      ls = list()
      ls[["start"]] = start_time
      ls[["finish"]] = end_time
      return(ls)
    },
    questFinished = function(quest){
      return(nrow(quest$data[quest$data$Action == "Quest finished",]) > 0)
    },
    selectQuestPositionData = function(quest,time_window){
      if(missing(time_window)) stop("Need to specify time window")
      if (length(time_window)!=2) stop("Time window needs to have only two times inside")
      id_of_set = (filter(self$quest_set, name == quest$name) %>% select(id_of_set))[[1]]
      position_table = self$trial_sets[[id_of_set]]$player_log
      return(position_table[Time > time_window$start & Time < time_window$finish])
    },
    getTeleportTimes = function(quest = NULL, quest_idx=NULL){
      if(is.null(quest)) quest = private$questStep(quest_idx)
      if(is.null(quest)) stop("Quest log not reachable")
      ls = list()
      ls[["start"]] = private$getStepTime(quest, "Teleport Player")
      ls[["finish"]] = private$getStepTime(quest, "Teleport Player", "StepFinished")
      return(ls)
    },
    getQuestStartAndFinishPositions = function(quest, include_teleport = T){
      if(is.null(quest)) stop("Quest log not reachable")
      #gets finished time of the teleport
      teleport_finished = private$getTeleportTimes(quest)$finish
      teleport_target_postition = private$playerLogForQuest(quest)[Time > teleport_finished, .SD[1,c(Position.x,Position.z)]]
      #goes step by step from the end until it gets end with transform
      for(i in nrow(quest$steps):1){
        quest_finish_position = text_to_vector3(quest$steps$Transform[i])
        if(!is.null(quest_finish_position)){
          quest_finish_position = quest_finish_position[c(1,3)]
          break
        }
      }
      ls = list()
      ls[["start"]] = teleport_target_postition
      ls[["finish"]] = quest_finish_position
      return(ls)
    },
    wholePlayerLog = function(){
      player_log = data.table()
      for(i in 1:length(self$trial_sets)){
        pos_tab =  self$trial_sets[[i]]$player_log
        player_log = rbindlist(list(player_log,pos_tab))
      }
      return(player_log)
    },
    playerLogForQuest = function(quest){
      quest_line = filter(self$quest_set, name == quest$name)
      if(nrow(quest_line) >1) stop("Multiple quests have the same name")
      quest_times = private$getQuestTimewindow(quest, include_teleport = T)
      player_log = self$trial_sets[[quest_line$id_of_set]]$player_log[Time > quest_times$start & Time < quest_times$finish,]
      return(player_log)
    },
    getStepTime = function(quest, step_name, step_action = "StepActivated", step_id = NULL){
      if(is.null(quest))stop("Quest log not reachable")
      if(!is.null(step_id)) return(quest$data$TimeFromStart[quest$data$StepID == step_id & quest$data$Action == step_action])
      steps = quest$data$TimeFromStart[quest$data$StepType == step_name & quest$data$Action == step_action]
      if(length(steps) > 1) stop ("There is more steps of the same parameters. Specify please")
      return(steps)
    },
    mapSize = function(quest = NULL){
      ls = list()
      if (is.null(quest)) quest = private$questStep(1)
      quest_line = filter(self$quest_set, name == quest$name)
      terrain_info = self$trial_sets[[quest_line$id_of_set]]$experiment_log$terrain
      size = text_to_vector3(terrain_info$Size)
      pivot = text_to_vector3(terrain_info$Pivot)
      ls[["x"]] = c(pivot[1],pivot[1] + size[1])
      ls[["y"]] = c(pivot[3],pivot[3] + size[3])
      return(ls)
    }
  )
)
