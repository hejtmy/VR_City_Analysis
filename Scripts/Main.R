source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir = "../Data/"
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv",sep = ""), sep = ",", 
                           header = T, stringsAsFactors = F, na.strings = c(""))

SESSION = 2

#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)

Analyses$load_unity_eyetracker()
tabEyeQuests = Analyses$EyetrackerQuestsSummary()

Analyses$load_unity_mri()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$SynchropulsesTable()

Analyses$load_eyetracker()
synced_fixations = Analyses$synchronise_eyetracker()
dt_pointing = Analyses$pointing_summary()

write.table(tabEyeQuests,"tabEyeQuests_session2.csv", sep = ";", row.names = F, quote = F)
write.table(tabMRIQuests,"tabMRIQuests_session2.csv", sep = ";", row.names = F, quote = F)
write.table(dt_pointing, "pointing_session2.csv", sep = ";", row.names = F, quote = F)

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
write.table(synced_fixations, "synced_fixations_session2.csv", sep = ";", row.names = F, quote = F)

fix_summary = make_fixations_summary(dt)
