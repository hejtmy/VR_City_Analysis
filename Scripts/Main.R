source("Scripts/LoadingScript.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data/"

subject_table = read.table("../Data/ListOfSubjects.csv", sep = ";", header=T, stringsAsFactors = F, na.strings = c(""))

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
Analysis = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
Analysis$MakePathImage(8)
Analysis$QuestSummary(7)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(2)

#MRI analysis
Analysis =  UnityMRIAnalysis$new(data_dir,id="HCE_1_E_2")
Analysis$MakePathImage(8)
Analysis$QuestSummary(2)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(1)

#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir,subject_table,1)
tab = Analyses$EyetrackerQuestsSummary()
tabMRI = Analyses$MRIQuestSummary()

GetNumberOfPulses(Analyses)

t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, tab))
summary(aov(time~type*participant_id, tab))
