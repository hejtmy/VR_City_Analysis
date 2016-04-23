source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data"

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
UnityAnal = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
UnityAnal$ReadData()
UnityAnal$MakePathImage(7)
UnityAnal$QuestSummary(7)
UnityAnal$QuestsSummary()

UnityAnal$DrawQuestParth(2)

UnityAnal$QuestSummary(quest_session_idx = 7)

#choose participants
participants = c("HCE_1_E_1","HCE_1_E_2","HCE_1_E_3","HCE_1_E_4","HCE_1_E_5")

Analyses = MultiParticipantUnityAnalysis$new(data_dir,participants,1)
tab = Analyses$QuestsSummary()

t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, tab))
summary(aov(time~type*participant_id, tab))
