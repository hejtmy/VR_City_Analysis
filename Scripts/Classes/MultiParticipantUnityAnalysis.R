MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
 #define variables
 public = list(
    Data = NULL,
    initialize = function(dir, subject_table, session){
      for(i in 1:nrow(subject_table)){
        participant_code = subject_table$ID.NUDZ[i]
        unity_code = subject_table$VR_EYE_1[i]
        if(is.na(unity_code)){
          print("------------")
          SmartPrint(c("There is no unity log for participant", participant_code))
        } else {
          SmartPrint(c("------------ Loading", participant_code,"------------"))
          SmartPrint(c("Code for Eyetracker log for participant", participant_code, "is", unity_code))
          analysis = UnityEyetrackerAnalysis$new(dir, participant_code, session)
          self$Data[[participant_code]]$UnityEyetracker = analysis
        }
        mri_code = subject_table$VR_MRI_1[i]
        if(is.na(mri_code)){
          print("------------")
          SmartPrint(c("There is no MRI log for participant", participant_code))
        } else {
          SmartPrint(c("Code for MRI log for participant", participant_code, "is", mri_code))
          analysis = UnityMRIAnalysis$new(dir,participant_code)
          self$Data[[participant_code]]$MRI = analysis
        }
        ##eyetracker loading
      }
    },
    EyetrackerQuestsSummary = function(force = F){
     if (!force & !is.null(private$eyetracker_quest_summary_tab)) return (private$eyetracker_quest_summary_tab)
     final = data.frame()
     for(i in 1:length(self$Data)){
       print(i)
       analysis = self$Data[[i]]$UnityEyetracker
       if(is.null(analysis)) next
       df = analysis$QuestsSummary()
       df = mutate(df, participant_id = rep(analysis$id,nrow(df)))
       final = rbindlist(list(final,df))
     }
     private$eyetracker_quest_summary_tab = final
     return(final)
    },
    MRIQuestSummary = function(force = F){
      if (!force & !is.null(private$mri_quest_summary_tab)) return (private$mri_quest_summary_tab)
      final = data.frame()
      for(i in 1:length(self$Data)){
        print(i)
        analysis = self$Data[[i]]$MRI
        if(is.null(analysis)) next
        df = analysis$QuestsSummary()
        df = mutate(df, participant_id = rep(analysis$id,nrow(df)))
        final = rbindlist(list(final,df))
      }
      private$mri_quest_summary_tab = final
      return(final)
    },
    SynchropulsesTable = function(force = F){
      if (!force & !is.null(private$synchro_table)) return (private$synchro_table)
      private$synchro_table = MultiMRIPulsesTable(self)
    }
 ),
 private = list(
   eyetracker_quest_summary_tab = NULL,
   mri_quest_summary_tab = NULL,
   synchro_table =NULL
 )
)
WorstPeopleEyetracker = function(MultiParticipantUnityAnalysis){
  tab = MultiParticipantUnityAnalysis$EyetrackerQuestsSummary()
  tab2 = tab[,.(max_distance = max(distance)), by=id]
  comparison_tab = merge(tab,tab2, by.x="distance",by.y="max_distance")
  return(comparison_tab)
}
WorstPeopleMRI = function(MultiParticipantUnityAnalysis){
  tab = MultiParticipantUnityAnalysis$MRIQuestSummary()
  tab2 = tab[,.(max_distance = max(distance)), by=id]
  comparison_tab = merge(tab,tab2, by.x="distance",by.y="max_distance")
  return(comparison_tab)
}