UnityEyetrackerAnalysis <- R6Class("UnityEyetrackerAnalysis",
    inherit = BaseUnityAnalysis,
    #define variables
    public = list(
        #basic definitions
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
      setDataDirectory = function(){
        self$data_directory <- paste(self$dir,self$id,"VR",self$session,sep="/")
      }
    )
)
