# mri_pointing_pulses

source("Scripts/LoadingScript.R")

data_dir = "../Data/"
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv", sep = ""), sep = ",", 
                           header = T, stringsAsFactors = F, na.strings = c(""))

SESSION = 1

correct_angles = read.csv(paste(data_dir, "correct-angles.csv", sep = ""),
                          header = T, stringsAsFactors = F, na.strings = c(""))
#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)
Analyses$load_unity_mri()

ls = list()
for (i in 1:length(Analyses$Data)){
  
  Anal = Analyses$Data[[i]]$MRI
  if(is.null(Anal)) next
  print(Anal$id)
  
  pulses_times = mri_pulses_table(Anal)
  if(is.null(pulses_times) || pulses_times[,.N] == 0) next
  
  pulses_times[, c(2:12):= NULL]
  pulses_times[, i := 1:.N]
  
  pointing_summary = Anal$pointing_summary(correct_angles = correct_angles, override = T)
  if(is.null(pointing_summary) || pointing_summary[, .N] == 0) next
  n_row = pointing_summary[, .N]
  pointing_summary[, start_pulse := rep(as.numeric(NA), n_row)]
  pointing_summary[, start_pulse_time := rep(as.numeric(NA), n_row)]
  pointing_summary[, last_pulse := rep(as.numeric(NA), n_row)]
  pointing_summary[, last_pulse_time := rep(as.numeric(NA), n_row)]
  
  for (n in 1:nrow(pointing_summary)){
    # find the last matching start pulse
    i_start = tail(which(pulses_times$Time < pointing_summary$point_start[n]), 1)
    if(length(i_start) > 0){
      pointing_summary[n, start_pulse := pulses_times$i[i_start]]
      pointing_summary[n, start_pulse_time := pulses_times$Time[i_start]]
    }
    i_end = tail(which(pulses_times$Time < pointing_summary$point_end[n]), 1)
    if(length(i_start) > 0){
      pointing_summary[n, last_pulse := pulses_times$i[i_end]]
      pointing_summary[n, last_pulse_time := pulses_times$Time[i_end]]
    }
  }
  ls[[Anal$id]] = pointing_summary
}

pulses_pointing = rbindlist(ls, fill = T)

write.table(pulses_pointing,"pulses_pointing_session1.csv", sep = ";", row.names = F, quote = F)
