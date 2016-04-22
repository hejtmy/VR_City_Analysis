source("Scripts/Classes/EyetrackerUnityAnalysis.R")
source("Scripts/Classes/MultiParticipantUnityAnalysis.R")
source("Scripts/Classes/VUZIX_analysis.R")
source("Scripts/Eyetracker/importing.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data"

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
UnityAnal = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
UnityAnal$ReadData()
UnityAnal$MakePathImage(7)
UnityAnal$QuestSummary(7)
UnityAnal$QuestsSummary()

#choose participants
participants = c("HCE_1_E_1","HCE_1_E_2","HCE_1_E_3","HCE_1_E_4","HCE_1_E_5")

anal = MultiParticipantUnityAnalysis$new(data_dir,participants,1)
tab = anal$QuestsSummary()

t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, ab))
summary(aov(time~type*participant_id, ab))
