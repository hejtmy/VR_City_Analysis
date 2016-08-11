source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data/"
data_dir = "C:/Users/hejtmy/OneDrive/NUDZ/HCENAT/Data/"
data_dir= "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv",sep=""), sep = ",", header=T, stringsAsFactors = F, na.strings = c(""))

subject_table = subject_table[1:10,]

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
AnalysisEye = UnityEyetrackerAnalysis$new(data_dir, id = "HCE_E_10", session = 1)
AnalysisEye$QuestSummary(7)
AnalysisEye$QuestsSummary()
AnalysisEye$DrawQuestPath(2)
AnalysisEye$pointing_summary()

#MRI analysis
AnalysisMRI = UnityMRIAnalysis$new(data_dir, id = "HCE_E_10", session = 1)
AnalysisMRI$QuestSummary(2)
AnalysisMRI$QuestsSummary()
AnalysisMRI$DrawQuestPath(1)
AnalysisMRI$pointing_summary()


#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, 1, override = F)
tabEyeQuests = Analyses$EyetrackerQuestsSummary()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$SynchropulsesTable()
synced_fixations = Analyses$synchronise_eyetracker()
dt_pointing = Analyses$pointing_summary()

write.table(tabEyeQuests,"tabEyeQuests.csv", sep=";", row.names = F, quote = F)
write.table(tabMRIQuests,"tabMRIQuests.csv", sep=";", row.names = F, quote = F)
write.table(synced_fixations,"synced_fixations.csv", sep=";", row.names = F, quote = F)
write.table(dt_pointing,"pointing.csv", sep=";", row.names = F, quote = F)

MRIInformation(tabMRIPulses)

t.test(tabEyeQuests$time~tabEyeQuests$type)
#anova model
summary(aov(time~id, tabEyeQuests))
summary(aov(time~type*participant_id, tabEyeQuests))
