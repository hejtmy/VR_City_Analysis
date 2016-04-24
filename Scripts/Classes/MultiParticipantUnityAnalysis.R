MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
                                         
 #define variables
 public = list(
    Analyses = NULL,
    initialize = function(dir, subject_table,session){
      Analyses = list()
      for(i in 1:nrow(subject_table)){
        participant_code = subject_table$Code[i]
        unity_code = subject_table$UnityCode[i]
        if(is.na(unity_code)){
          print("------------")
          SmartPrint(c("There is no unity log for participant", participant_code))
          next
        }
        SmartPrint(c("------------ Loading", participant_code,"------------"))
        SmartPrint(c("Code for participant", participant_code, "is", unity_code))
        analysis = UnityEyetrackerAnalysis$new(dir,unity_code,session)
        self$Analyses[[participant_code]] = analysis
      }
   },
   QuestsSummary = function(force = F){
     if (!force & !is.null(private$quest_summary_tab)) return (private$quest_summary_tab)
     final = data.frame()
     for(i in 1:length(self$Analyses)){
       print(i)
       ana = self$Analyses[[i]]
       df = ana$QuestsSummary()
       df = mutate(df, participant_id = rep(ana$id,nrow(df)))
       final = rbindlist(list(final,df))
     }
     private$quest_summary_tab = final
     return(final)
   },
   WorstPeople = function(){
     
   }
 ),
 private = list(
   quest_summary_tab = NULL
 )
)