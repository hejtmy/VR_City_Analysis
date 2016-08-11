MultiParticipantUnityAnalysis <- R6Class("MultiParticipantUnityAnalysis",
  #define variables
  public = list(
    Data = NULL,
    session = NULL,
    
    #initialisation
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
      
      #for each participant
      for(i in 1:nrow(subject_table)){
        participant_code = subject_table$ID[i]
        
        # ------- UNITY ---------
        unity_code = subject_table$VR_EYE[i]
        if(is.na(unity_code)){
          print("------------")
          SmartPrint(c("There is no unity log for participant", participant_code))
        } else {
          SmartPrint(c("------------ Loading", participant_code,"------------"))
          SmartPrint(c("Code for Eyetracker log for participant", participant_code, "is", unity_code))
          analysis = UnityEyetrackerAnalysis$new(dir, participant_code, session)
          if (!is.null(analysis) && analysis$valid()){
            self$Data[[participant_code]]$UnityEyetracker = analysis
          }
        }
        
        # ------- EYETRACKER ---------
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
        
        # ------- MRI ---------
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
    synchronise_eyetracker = function(override = F){
      #should possibly save?
      if (!override & !is.null(private$fixations_synchronised)) return (private$fixations_synchronised)
      final = data.table()
      for(i in 1:length(self$Data)){
        print(self$Data[[i]]$UnityEyetracker$eyetracker$data_directory)
        eyetracker = self$Data[[i]]$UnityEyetracker$eyetracker
        if(is.null(eyetracker)) next
        
        unity_class = self$Data[[i]]$UnityEyetracker
        if(is.null(unity_class)){
          SmartPrint(c("WARNING:MultiParticipantUnityAnalysis:EyetrackerSummary:NoQuestTimes", "ID:", eyetracker$id, "DESCRIPTION: You need to run EyetrackerQuestSummary first"))
          next
        }
        dt = eyetracker$synchronise(unity_class, override)
        if(is.null(dt)) next
        dt[, participant_id := eyetracker$id]
        final = rbindlist(list(final, dt))
      }
      private$fixations_synchronised = final
      return(private$fixations_synchronised)
    },
    
    pointing_summary = function(override = F){
      if (!override & !is.null(private$dt_pointing_summary)) return (private$dt_pointing_summary)
      dt_final = data.table()
      
      #unity pointing
      for(i in 1:length(self$Data)){
        analysis = self$Data[[i]]$UnityEyetracker
        print(self$Data[[i]]$UnityEyetracker$data_directory)
        if(is.null(analysis)){
          SmartPrint(c("WARNING:pointing_summary:MissingData","TYPE:UnityClass", "ACTION:Skipping"))
          next
        }
        dt = analysis$pointing_summary(override)
        if(is.null(dt)) next
        dt[, participant_id := analysis$id]
        dt_final = rbindlist(list(dt_final, dt))
      }
      
      # MRI POINTING
      for(i in 1:length(self$Data)){
        analysis = self$Data[[i]]$MRI
        print(self$Data[[i]]$MRI$data_directory)
        if(is.null(analysis)){
          SmartPrint(c("WARNING:pointing_summary:MissingData","TYPE:MRI", "ACTION:Skipping"))
          next
        }
        dt = analysis$pointing_summary(override)
        if(is.null(dt)) next
        dt[, participant_id := analysis$id]
        dt_final = rbindlist(list(dt_final, dt))
      }
      
      private$dt_pointing_summary = dt_final
      return(private$dt_pointing_summary)
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
    fixations_synchronised = NULL,
    mri_quest_summary_tab = NULL,
    synchro_table = NULL,
    dt_pointing_summary = NULL
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