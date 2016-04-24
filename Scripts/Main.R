source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data/"

subject_table = read.table("../Data/ListOfSubjects.csv", sep = ";", header=T, stringsAsFactors = F, na.strings = c(""))
#choose participants
participants = subject_table$UnityCode[!is.na(subject_table$UnityCode)]

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
UnityAnal = UnityEyetrackerAnalysis$new(data_dir,id=participants[7],session=1)
UnityAnal$MakePathImage(8)
UnityAnal$QuestSummary(7)
UnityAnal$QuestsSummary()
UnityAnal$DrawQuestPath(2)

UnityMRI = UnityMRIAnalysis$new(data_dir,id="HCE_1_E_2")
UnityMRI$QuestsSummary()
UnityMRI$MakePathImage(6)

Analyses = MultiParticipantUnityAnalysis$new(data_dir,subject_table,1)
tab = Analyses$EyetrackerQuestsSummary()
tabMRI = Analyses$MRIQuestSummary()

GetNumberOfPulses(Analyses)

t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, tab))
summary(aov(time~type*participant_id, tab))
