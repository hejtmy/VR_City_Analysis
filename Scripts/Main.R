source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data/"
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir= "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv",sep=""), sep = ",", header=T, stringsAsFactors = F, na.strings = c(""))

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
Analysis = UnityEyetrackerAnalysis$new(data_dir, id="HCE_E_12", session=1)
Analysis$QuestSummary(7)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(2)

#MRI analysis
Analysis = UnityMRIAnalysis$new(data_dir, id="HCE_K_10", session=1)
Analysis$QuestSummary(2)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(1)

#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, 1, override = F)
tabEyeQuests = Analyses$EyetrackerQuestsSummary()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$SynchropulsesTable()
synced_fixations = Analyses$synchronise_eyetracker()

write.table(tabEyeQuests,"tabEyeQuests.csv", sep=";", row.names = F, quote = F)
write.table(tabMRIQuests,"tabMRIQuests.csv", sep=";", row.names = F, quote = F)
write.table(synced_fixations,"synced_fixations", sep=";", row.names = F, quote = F)

MRIInformation(tabMRIPulses)

t.test(tabEyeQuests$time~tabEyeQuests$type)
#anova model
summary(aov(time~id, tabEyeQuests))
summary(aov(time~type*participant_id, tabEyeQuests))
