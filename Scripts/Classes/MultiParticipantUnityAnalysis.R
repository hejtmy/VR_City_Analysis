MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
  #define variables
  public = list(
    Data = NULL,
    session = NULL,
    initialize = function(dir = NULL, subject_table = NULL, session = NULL, data=NULL, override = F, save = T){
      self$session = session
      #allows to preloade data
      if(!is.null(data)){
        self$Data = data
        return(self) 
      }
      #if saved
      self$Data = list()
      ptr = paste("_", session, sep = "", collapse = "")
      subject_table = select(subject_table, ID, contains(ptr))
      names(subject_table) = sapply(names(subject_table), function(x) gsub(x, pattern = ptr, replacement = "" ))
      for(i in 1:nrow(subject_table)){
        participant_code = subject_table$ID[i]
        unity_code = subject_table$VR_EYE[i]
        if(is.na(unity_code)){
          print("------------")
          SmartPrint(c("There is no unity log for participant", participant_code))
        } else {
          SmartPrint(c("------------ Loading", participant_code,"------------"))
          SmartPrint(c("Code for Eyetracker log for participant", participant_code, "is", unity_code))
          analysis = UnityEyetrackerAnalysis$new(dir, participant_code, session)
          if (!is.null(analysis)) self$Data[[participant_code]]$UnityEyetracker = analysis
        }
        ##eyetracker loading
        edf_code = subject_table$EDF_EYE[i]
        if(is.na(edf_code)){
          print("------------")
          SmartPrint(c("There is no edf file for participant", participant_code))
        } else {
          SmartPrint(c("Code for edf log for participant", participant_code, "is", edf_code))
          if(!is.null(self$Data[[participant_code]]$UnityEyetracker)){
            eye = EyetrackerAnalysis$new(dir, participant_code, edf_code, 
                                         unity_class = self$Data[[participant_code]]$UnityEyetracker, 
                                         override, save)
            if(eye$valid()) self$Data[[participant_code]]$UnityEyetracker$eyetracker = eye
          }
        }
        mri_code = subject_table$VR_MRI[i]
        if(is.na(mri_code)){
          print("------------")
          SmartPrint(c("There is no MRI log for participant", participant_code))
        } else {
          SmartPrint(c("Code for MRI log for participant", participant_code, "is", mri_code))
          analysis = UnityMRIAnalysis$new(dir,participant_code, session)
          self$Data[[participant_code]]$MRI = analysis
        }
      }
    },
    EyetrackerQuestsSummary = function(force = F){
     if (!force & !is.null(private$eyetracker_quest_summary_tab)) return (private$eyetracker_quest_summary_tab)
     final = data.frame()
     for(i in 1:length(self$Data)){
       print(self$Data[[i]]$UnityEyetracker$data_directory)
       analysis = self$Data[[i]]$UnityEyetracker
       if(is.null(analysis)) next
       df = analysis$QuestsSummary(force)
       if(is.null(df)) next
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
        print(self$Data[[i]]$MRI$data_directory)
        analysis = self$Data[[i]]$MRI
        if(is.null(analysis)) next
        df = analysis$QuestsSummary(force)
        if(is.null(df)) next
        df = mutate(df, participant_id = rep(analysis$id,nrow(df)))
        final = rbindlist(list(final,df))
      }
      private$mri_quest_summary_tab = final
      return(final)
    },
    EyetrackerSummary = function(force = F){
      if (!force & !is.null(private$eyetracker_summary_tab)) return (private$eyetracker_summary_tab)
      final = data.frame()
      for(i in 1:length(self$Data)){
        print(self$Data[[i]]$UnityEyetracker$eyetracker$data_directory)
        eyetracker = self$Data[[i]]$UnityEyetracker$eyetracker
        if(is.null(eyetracker)) next
        unity_class = self$Data[[i]]$UnityEyetracker$quests_timewindows(include_teleport = T)
        if(is.null(quest_times)){
          SmartPrint(c("WARNING:MultiParticipantUnityAnalysis:EyetrackerSummary:NoQuestTimes", "ID:", eyetracker$id, "DESCRIPTION: You need to run EyetrackerQuestSummary first"))
          return(NULL)
        }
        df = eyetracker$summary(force, unity_class)
        if(is.null(df)) next
        df = mutate(df, participant_id = rep(eyetracker$id, nrow(df)))
        final = rbindlist(list(final, df))
      }
      private$eyetracker_summary_tab = final
      return(final)
    },
    SynchropulsesTable = function(force = F){
      if (!force & !is.null(private$synchro_table)) return (private$synchro_table)
      private$synchro_table = MultiMRIPulsesTable(self)
    },
    draw_quest_participants = function(experiment = NULL, quest_id, types = c("trial"), map_img_path = NULL){
      data = select_experiment(self$Data, experiment)
      draw_quest_participants(data, quest_id, types, map_img_path)
    },
    select_experiment = function(experiment = NULL){
      allowed_types = c("UnityEyetracker", "MRI")
      if(!(experiment %in% allowed_types)){
        SmartPrint(c("ERROR: experiment can be of following types", allowed_types))
        stop()
      }
      return(select_experiment(self$Data, experiment))
    }
  ),
  private = list(
    eyetracker_quest_summary_tab = NULL,
    eyetracker_summary_tab = NULL,
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