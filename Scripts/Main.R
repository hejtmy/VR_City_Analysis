#setwd("C:/Users/hejtmy/Documents/NUDZ/AnalysisR")
#NUDZ settings
#setwd("D:/VR/VRcity/VR_City_Analysis")

source("Scripts/Classes/EyetrackerUnityAnalysis.R")
source("Scripts/Classes/VUZIX_analysis.R")
source("Scripts/Eyetracker/importing.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data"

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
UnityAnal = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_1",session=1)
UnityAnal$ReadData()
UnityAnal$MakePathImage(20)
UnityAnal$QuestSummary(7)
UnityAnal$QuestsSummary()

UnityAnal3 = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
UnityAnal3$ReadData()
UnityAnal3$MakePathImage(20)
UnityAnal3$QuestSummary(7)
#cannot do this until the scenario log had been figured out. Need more information of how the data will look
#SumTime(analysis)

#eytracker stuff
result <- parse.asc.file(filepath)
data = result$data
data <- data[X < 1920 & X > 0]
data <- data[Y < 1280 & Y > 0]
gg = ggplot(data[5000:50000],aes(X,Y))
gg + geom_path()
