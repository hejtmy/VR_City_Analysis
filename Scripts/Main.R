setwd("C:/Users/hejtmy/Documents/NUDZ/AnalysisR")
#NUDZ settings
#setwd("D:/VR/VRcity/SVN/AnalysisR")

source("VR_analysis.R")

source("VUZIX_analysis.R")

#sets the working dirrectory where the logs are
project_dir <- "C:/Users/hejtmy/Documents/NUDZ/Project/Unity/sw-main-u5/Assets/logs"

#kostel build
project_dir <- "D:/VR/VRcity/Builds/NemocniceKostel_Data/logs"

#NUDZ settings
#sets the working dirrectory where the logs are
project_dir <- "D:/VR/VRcity/SVN/Project/Unity/sw-main-u5/Assets/logs"

#reads a VUZIX log and parses the data
vz = VUZIX_analysis("C:/Users/hejtmy/Documents/NUDZ/AnalysisR","18072015.csv")

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
analysis <- VR_analysis(dir = project_dir, code="NEO", timestamp="10-43-34-01-12-2015")

MakePathImage(analysis,"GoodMapIGuess2.png")

#cannot do this until the scenario log had been figured out. Need more information of how the data will look
#SumTime(analysis)