source("R/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir = "../Data/"
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv", sep = ""), sep = ",", 
                           header = T, stringsAsFactors = F, na.strings = c(""))

SESSION = 1

subject_table = subject_table[c(16,18),]
#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)

# -------- UNITY EYE ---------
Analyses$load_unity_eyetracker()
tabEyeQuests = Analyses$EyetrackerQuestsSummary()
write.table(tabEyeQuests,"tabEyeQuests_session1.csv", sep = ";", row.names = F, quote = F)

# -------- UNITY MRI ---------
Analyses$load_unity_mri()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$synchropulses_table()
write.table(tabMRIQuests, "tabMRIQuests_session1.csv", sep = ";", row.names = F, quote = F)

# -------- EYETRACKER ---------
Analyses$load_eyetracker()
synced_fixations = Analyses$synchronise_eyetracker()

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
write.table(synced_fixations, "synced_fixations_session1.csv", sep = ";", row.names = F, quote = F)

# -------- POINTING ---------
correct_angles = read.csv(paste(data_dir, "correct-angles.csv", sep = ""),
                          header = T, stringsAsFactors = F, na.strings = c(""))
dt_pointing = Analyses$pointing_summary(override = T, correct_angles = correct_angles)
write.table(dt_pointing, "pointing_session1.csv", sep = ";", row.names = F, quote = F)

fix_summary = make_fixations_summary(dt)