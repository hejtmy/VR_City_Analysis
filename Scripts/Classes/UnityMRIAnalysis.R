UnityMRIAnalysis <- R6Class("UnityMRIAnalysis",
  inherit = BaseUnityAnalysis,
  #define variables
  public = list(
   QuestsSummary = function(force = F){
     if (!force & !is.null(private$quests_summary)) return (private$quests_summary)
     df = super$QuestsSummary()
     firstPulses = integer(nrow(df))
     numberOfPulses = integer(nrow(df))
     for(i in 1:nrow(df)){
       quest = private$questStep(i)
       mri_summary = private$calculateMRIPulses(quest)
       firstPulses[i] = ifelse(length(mri_summary$firstPulse)<1, NA, mri_summary$firstPulse)
       numberOfPulses[i] = ifelse(length(mri_summary$numberOfPulses)<1, NA, mri_summary$numberOfPulses)
     }
     df = mutate(df, firstPulse = firstPulses, numberOfPulses = numberOfPulses)
     private$quests_summary = df
     return(df)
   }
  ),
  private = list(
    isValid = function(){
    },
    setDataDirectory = function(){
      self$data_directory = paste(self$dir, self$id, "MRI", self$session, sep="/")
    },
    #calculates order and number of fMRI pulses
    calculateMRIPulses = function(quest){
      ls = list()
      player_log = private$wholePlayerLog()
      timeWindow = private$getQuestTimewindow(quest, include_teleport = T)
      #gets the index numbers of the rows in the quest time range
      #indexes are not from the entire log, but only from the fMRI sychnro line
      pulses = player_log[Input == "fMRISynchro",.I[Time > timeWindow$start & Time < timeWindow$finish]]
      ls$numberOfPulses = length(pulses)
      ls$firstPulse = pulses[1]
      return(ls)
    }
  )
)
