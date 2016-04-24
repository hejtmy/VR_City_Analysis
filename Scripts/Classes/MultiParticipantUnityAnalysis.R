MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
 #define variables
 public = list(
    Data = NULL,
    initialize = function(dir, subject_table,session){
      Data = list()
      for(i in 1:nrow(subject_table)){
        participant_code = subject_table$Code[i]
        unity_code = subject_table$UnityCode[i]
        if(is.na(unity_code)){
          print("------------")
          SmartPrint(c("There is no unity log for participant", participant_code))
          next
        }
        SmartPrint(c("------------ Loading", participant_code,"------------"))
        SmartPrint(c("Code for Eyetracker log for participant", participant_code, "is", unity_code))
        analysis = UnityEyetrackerAnalysis$new(dir,participant_code,session)
        self$Data[[participant_code]]$UnityEyetracker = analysis
        
        mri_code = subject_table$MRICode[i]
        if(is.na(mri_code)){
          print("------------")
          SmartPrint(c("There is no MRI log for participant", participant_code))
          next
        }        
        SmartPrint(c("Code for MRI log for participant", participant_code, "is", mri_code))
        analysis = UnityMRIAnalysis$new(dir,participant_code)
        self$Data[[participant_code]]$MRI = analysis
      }
    },
    QuestsSummary = function(force = F){
     if (!force & !is.null(private$quest_summary_tab)) return (private$quest_summary_tab)
     final = data.frame()
     for(i in 1:length(self$Data)){
       print(i)
       ana = self$Data[[i]]$UnityEyetracker
       df = ana$QuestsSummary()
       df = mutate(df, participant_id = rep(ana$id,nrow(df)))
       final = rbindlist(list(final,df))
     }
     private$quest_summary_tab = final
     return(final)
    },
    WorstPeople = function(){
     if(is.null(private$quest_summary_tab)) self$QuestsSummary()
     tab = private$quest_summary_tab
     tab2 = tab[,.(max_distance = max(distance)), by=id]
     comparison_tab = merge(tab,tab2, by.x="distance",by.y="max_distance")
     return(comparison_tab)
    }
 ),
 private = list(
   quest_summary_tab = NULL
 )
)