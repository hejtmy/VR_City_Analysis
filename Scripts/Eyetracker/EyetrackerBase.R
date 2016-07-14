EyetrackerBase = R6Class('EyetrackerBase',
  public = list(
    data = NULL,
    initialize = function(path){
      private$read_eye_data(path)
    }
  ),
  private = list(
    read_eye_data = function(path){
      data = read_eye_data(path)
      data = preprocess_eye_data(data)
      self$data = data
    }
  )
)
