source("R/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

SESSION = 1

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv", sep = ""), sep = ",", header = T, stringsAsFactors = F, na.strings = c(""))

#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)
tabEyeQuests = Analyses$EyetrackerQuestsSummary()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$SynchropulsesTable()
synced_fixations = Analyses$synchronise_eyetracker()
dt_pointing = Analyses$pointing_summary()

areas = create_areas()
dt = add_screen_area_fixations(synced_fixations, areas)
fix_summary = make_fixations_summary(dt)

tab_eye_name = paste("tabEyeQuests_session", SESSION, ".csv", sep = "", collapse = "")
tab_mri_name = paste("tabMRIQuests_session", SESSION, ".csv", sep = "", collapse = "")
synced_fixations_name = paste("synced_fixations_session", SESSION, ".csv", sep = "", collapse = "")
pointing_name = paste("pointing_session", SESSION, ".csv", sep = "", collapse = "")
fixations_summary_name = paste("fixations_summary_session", SESSION, ".csv", sep = "", collapse = "")

write.table(tabEyeQuests, tab_eye_name, sep = ";", row.names = F, quote = F)
write.table(tabMRIQuests, tab_mri_name, sep = ";", row.names = F, quote = F)
write.table(synced_fixations, synced_fixations_name, sep = ";", row.names = F, quote = F)
write.table(dt_pointing, pointing_name, sep = ";", row.names = F, quote = F)
write.table(fix_summary, fixations_summary_name, sep = ";", row.names = F, quote = F)
