EyetrackerAnalysis = R6Class('EyetrackerAnalysis',
  inherit = BaseAnalysis,
  public = list(
    screen_resolution = NULL,
    fixations = NULL,
    fixations_synced = NULL,
    events = NULL,
    file = NULL,
    initialize = function(dir, id, file, unity_class = NULL, override = F, save = T){
      super$initialize(dir, id)
      self$file = file
      private$read_data(override)
      private$preprocess_data(save, unity_class)
    },
    valid = function(){
      return(!(is.null(self$events) || is.null(self$fixations)))
    },
    synchronise = function(unity_class, override = F){
      valid = T
      if(!self$valid()){
        SmartPrint(c("ERROR:EyetrackerAnalysis:synchronise:NotInitialised"))
        valid = F
      }
      if(is.null(unity_class)){
        SmartPrint(c("ERROR:EyetrackerAnalysis:synchronise:NotInitialised", "TYPE:UnityAnalysis"))
        valid = F
      }
      if (!valid) return(NULL)
      if (override || is.null(self$fixations_synced)){
        self$fixations_synced = synchronise_eye_unity(self$events, unity_class$event_times(), unity_class$quests_timewindows(T), self$fixations)
      }
      return(self$fixations_synced)
    },
    summary = function(force){
      if(is.null(self$fixations_synced)){
        SmartPrint(c("ERROR:EyetrackerAnalysis:summary:NotInitialised", "TYPE:fixations_synced"))
        valid = F
      }
    }
  ),
  private = list(
    set_directory = function(){
      self$data_directory = paste(self$dir, self$id, "Eyelink", self$session, sep="/")
    },
    read_data = function(override){
      private$set_directory()
      ls = read_eye_data(self$data_directory, self$file, override)
      if(is.null(ls)) return(F)
      self$fixations = ls$fixation
      self$events = ls$events
      self$screen_resolution = ls$resolution
      return(T)
    },
    preprocess_data = function(save, unity_class = NULL){
      # Event handeling
      if(is.null(self$events)){
        
      } else {
        #' this is different from handeling of player log because we need to remove rows and 
        #' data.table does not allow to remove rows by reference
        ls = prep_eye_events(self$events)
        self$events = ls$result
        if(ls$changed && save){
          filepath = paste(self$data_directory,  self$file, "_events.txt", sep = "")
          save_table(filepath, self$events)
        }
      }
      # Fixations handeling
      if(is.null(self$fixations)){
        
      } else {
        #' this is different from event handeling because we need to remove rows and 
        #' data.table does not allow to remove rows by reference
        ls = prep_eye_fixations(self$fixations, self$screen_resolution, unity_class)
        self$fixations = ls$result
        if(ls$changed && save){
          filepath = paste(self$data_directory, self$file, "_fixations.txt", sep = "")
          save_table(filepath, self$fixations)
        }
      }
    }
  )
)