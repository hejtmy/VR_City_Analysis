source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data/"

subject_table = read.table("../Data/ListOfSubjects.csv", sep = ";", header=T, stringsAsFactors = F, na.strings = c(""))
#choose participants
participants = subject_list$UnityCode[!is.na(subject_list$UnityCode)]

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
UnityAnal = UnityEyetrackerAnalysis$new(data_dir,id=participants[7],session=1)
#UnityAnal$ReadData()
UnityAnal$MakePathImage(8)
UnityAnal$QuestSummary(7)
UnityAnal$QuestsSummary()
UnityAnal$DrawQuestParth(2)

Analyses = MultiParticipantUnityAnalysis$new(data_dir,subject_table,1)
tab = Analyses$QuestsSummary()
Analyses$WorstPeople()

t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, tab))
summary(aov(time~type*participant_id, tab))
