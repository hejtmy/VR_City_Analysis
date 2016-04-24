UnityMRIAnalysis <- R6Class("UnityMRIAnalysis",
  inherit = BaseUnityAnalysis,
  #define variables
  public = list(
   initialize = function(dir=data_path, id=""){
     self$dir = dir
     self$SetParticipant(id)
     #TODO - check the data
     if(nargs() >= 2) {
       self$ReadData()
     }
   }
  ),
  private = list(
    isValid = function(){
    },
    setDataDirectory = function(){
      self$data_directory = paste(self$dir,self$id,"MRI",sep="/")
    }
  )
)
