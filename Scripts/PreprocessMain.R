source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

SESSION = 1

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv",sep=""), sep = ",", header=T, stringsAsFactors = F, na.strings = c(""))

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

write.table(tabEyeQuests, "tabEyeQuests_session1.csv", sep=";", row.names = F, quote = F)
write.table(tabMRIQuests, "tabMRIQuests_session1.csv", sep=";", row.names = F, quote = F)
write.table(synced_fixations, "synced_fixations_session1.csv", sep=";", row.names = F, quote = F)
write.table(dt_pointing, "pointing_session1.csv", sep=";", row.names = F, quote = F)
write.table(fix_summary, "fixations_summary_session1.csv", sep=";", row.names = F, quote = F)
