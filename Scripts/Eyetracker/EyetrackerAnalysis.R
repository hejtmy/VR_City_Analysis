EyetrackerAnalysis = R6Class('EyetrackerAnalysis',
  inherit = BaseAnalysis,
  public = list(
    fixations = NULL,
    events = NULL,
    file = NULL,
    initialize = function(dir, id, file, override = F, save = T){
      super$initialize(dir, id)
      self$file = file
      private$read_data(override)
      private$preprocess_data(save)
    },
    initialised = function(){
      return(!(is.null(self$events) || is.null(self$fixations)))
    },
    summary = function(force = F, quest_times){
      if(!self$initialised()){
        SmartPrint(c("ERROR:EyetrackerAnalysis:summary:NotInitialised"))
        return(NULL)
      }
      if(is.null(quest_times)){
        SmartPrint(c("ERROR:EyetrackerAnalysis:summary:MissingParameter", "TYPE:quest_times"))
        return(NULL)
      }
      return(eyetracker_summary(self$events, self$fixations, quest_times))
    }
  ),
  private = list(
    set_directory = function(){
      self$data_directory = paste(self$dir, self$id, "Eyelink", self$session, sep="/")
    },
    read_data = function(override){
      private$set_directory()
      ls = read_eye_data(self$data_directory, self$file, override)
      if(is.null(ls)) return()
      self$fixations = ls$fixation
      self$events = ls$events

    },
    preprocess_data = function(save){
      # Event handeling
      if(is.null(self$events)){
        
      } else {
        #' this is different from handeling of player log because we need to remove rows and 
        #' data.table does not allow to remove rows by reference
        ls = prep_eye_events(self$events)
        if(ls$changed){
          self$events = ls$result
          if(save){
            filepath = paste(self$data_directory,  self$file, "_events.txt", sep = "")
            save_table(filepath, self$events)
          }
        }
      }
      # Fixations handeling
      if(is.null(self$fixations)){
        
      } else {
        #' this is different from event handeling because we need to remove rows and 
        #' data.table does not allow to remove rows by reference
        ls = prep_eye_fixations(self$fixations)
        if(ls$changed){
          self$fixations = ls$result
          if(save){
            filepath = paste(self$data_directory, self$file, "_fixations.txt", sep = "")
            save_table(filepath, self$fixations)
          }
        }
      }
    }
  )
)