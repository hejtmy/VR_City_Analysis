#setwd("C:/Users/hejtmy/Documents/NUDZ/AnalysisR")
#NUDZ settings
#setwd("D:/VR/VRcity/VR_City_Analysis")

source("Scripts/Classes/VR_analysis.R")

source("Scripts/Classes/VUZIX_analysis.R")

source("Scripts/Eyetracker/importing.R")

#sets the working dirrectory where the logs are
project_dir <- "C:/Users/hejtmy/Documents/NUDZ/Project/Unity/sw-main-u5/Assets/logs"

#reads a VUZIX log and parses the data
vz = VUZIX_analysis$new(getwd(),"Testing","18072015.csv")

#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
anal = VR_analysis$new("U:/Vyzkum/VR_City_Analysis",id="id_1",session=2)
anal$SetTask(1)
MakePathImage(analysis,"GoodMapIGuess2.png")

#cannot do this until the scenario log had been figured out. Need more information of how the data will look
#SumTime(analysis)

#eytracker stuff
result <- parse.asc.file(filepath)
data = result$data
data <- data[X < 1920 & X > 0]
data <- data[Y < 1280 & Y > 0]
gg = ggplot(data[5000:50000],aes(X,Y))
gg + geom_path()
