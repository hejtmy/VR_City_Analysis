#libraries in correct order
library('R6')
library('data.table')
library('plyr')
library('dplyr')
library('stringr')
library('png')
library('grid')
library('ggplot2')

#custom items

source(paste(getwd(),"Scripts/HelperFunctions/helper_functions.R",sep="/"))
source_folder(paste(getwd(),"Scripts/HelperFunctions/",sep="/"))
source_folder(paste(getwd(),"Scripts/AnalysisFunctions/",sep="/"))
source_folder(paste(getwd(),"Scripts/Classes/",sep="/"))

source_folder(paste(getwd(),"Scripts/Eyetracker/", sep="/"))
