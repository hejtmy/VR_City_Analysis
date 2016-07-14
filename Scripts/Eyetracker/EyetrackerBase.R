EyetrackerBase = R6Class('EyetrackerBase',
  public = list(
    fixations = NULL,
    events = NULL,
    initialize = function(path){
      private$read_eye_data(path)
    }
  ),
  private = list(
    read_eye_data = function(path){
      ls = read_eye_data(path)
      ls = prep_eye_data(ls)
      self$fixations = ls$fixation
      self$events = ls$events
    }
  )
)
