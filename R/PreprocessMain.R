source("R/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

SESSION = 2

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv", sep = ""), sep = ",", header = T, stringsAsFactors = F, na.strings = c(""))

#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)
# -------- UNITY EYE ---------
Analyses$load_unity_eyetracker()
tabEyeQuests = Analyses$EyetrackerQuestsSummary()

# -------- UNITY MRI ---------
Analyses$load_unity_mri()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$synchropulses_table()

# -------- EYETRACKER ---------
Analyses$load_eyetracker()
synced_fixations = Analyses$synchronise_eyetracker()

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
fix_summary = make_fixations_summary(synced_fixations)

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
fix_summary = make_fixations_summary(synced_fixations)

# -------- POINTING ---------
correct_angles = read.csv(paste(data_dir, "correct-angles.csv", sep = ""),
                          header = T, stringsAsFactors = F, na.strings = c(""))
dt_pointing = Analyses$pointing_summary(override = T, correct_angles = correct_angles)

tab_eye_name = paste("tabEyeQuests_session", SESSION, ".csv", sep = "", collapse = "")
tab_mri_name = paste("tabMRIQuests_session", SESSION, ".csv", sep = "", collapse = "")
tab_mri_pulses_name = paste("tabMRIPulses_session", SESSION, ".csv", sep = "", collapse = "")
synced_fixations_name = paste("synced_fixations_session", SESSION, ".csv", sep = "", collapse = "")
pointing_name = paste("pointing_session", SESSION, ".csv", sep = "", collapse = "")
fixations_summary_name = paste("fixations_summary_session", SESSION, ".csv", sep = "", collapse = "")

write.table(tabEyeQuests, tab_eye_name, sep = ";", row.names = F, quote = F)
write.table(tabMRIQuests, tab_mri_name, sep = ";", row.names = F, quote = F)
write.table(tabMRIPulses, tab_mri_pulses_name, sep = ";", row.names = F, quote = F)
write.table(synced_fixations, synced_fixations_name, sep = ";", row.names = F, quote = F)
write.table(dt_pointing, pointing_name, sep = ";", row.names = F, quote = F)
write.table(fix_summary, fixations_summary_name, sep = ";", row.names = F, quote = F)
