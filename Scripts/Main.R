#setwd("C:/Users/hejtmy/Documents/NUDZ/AnalysisR")
#NUDZ settings
setwd("D:/VR/VRcity/VR_City_Analysis")

source("Scripts/Classes/VR_analysis.R")

source("Scripts/Classes/VUZIX_analysis.R")

#sets the working dirrectory where the logs are
project_dir <- "C:/Users/hejtmy/Documents/NUDZ/Project/Unity/sw-main-u5/Assets/logs"

#reads a VUZIX log and parses the data
vz = VUZIX_analysis$new(getwd(),"Testing","18072015.csv")

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
analysis <- VR_analysis(code="NEO")

MakePathImage(analysis,"GoodMapIGuess2.png")

#cannot do this until the scenario log had been figured out. Need more information of how the data will look
#SumTime(analysis)