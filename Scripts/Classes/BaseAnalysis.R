library('R6')
BaseAnalysis <- R6Class("BaseAnalysis",
   #define variables
   public = list(
     #basic definitions
     dir = NULL,
     id = NULL,
     
     initialize = function(dir, id){
       self$dir = dir
       self$SetParticipant(id)
     },
     SetParticipant = function(id=""){
       self$id = id
     },
     SetDataDir=function(dir=""){
       self$dir = dir
     }
   )
)
