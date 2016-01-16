source("preprocess_functions.R")
source("analysis_functions.R")

VUZIX_analysis <- setClass(
     "VUZIX_analysis",
     #define variables
     representation = list(
          #basic definitions
          dir = "character",
          log_file = "character",
          log_name = "character",
          
          #loaded tables and lists
          #GPS is a list of Status changes and data information of lattitude/longitude
          GPS = "list",
          accelometer = "data.frame",
          gyroscope = "data.frame",
          compass = "data.frame"
          
     ),
     
     #default values
     prototype = list(
          dir = paste(getwd(),"GPS_logs",sep="/")
     ),
     
     #define what is valid in the current context
     validity = function(object){
          #example
          #if (GPS == NULL) return(FALSE)
     }
)

setMethod(f = "initialize",
          signature = "VUZIX_analysis", 
          function(.Object, dir="", log_name=""){
               if(nargs() > 1) {
                    #if(length(x) != length(y))
                    #     stop("specified x and y of different lengths")
                    .Object@log_name <- log_name
                    .Object@log_file <- paste(dir,log_name,sep="/",collapse=NULL)
               }
               if(nargs() >= 2) {
                    data_list <- open_VUZIX_log(.Object@log_file)
                    .Object@GPS <- data_list$GPS
               }
               .Object
          }
)

open_VUZIX_log <- function(log){
     #needs to check if we got only one file out
     
     if(!file.exists(log)){
          print("Could not find the provided VUZIX log")
          return(NULL)
     }
     #prepares empty list
     data_list = list()
     #reads in the text
     text = readLines(log,warn=F)
     
     #gets the index of each line in the text file for each of the devices logged
     GPS_idx <- which(grepl("GPS",text))
     accelerometer_idx <- which(grepl("Accelerometer",text))
     compass_idx <- which(grepl("Compass",text))
     gyroscope_idx <- which(grepl("Gyroscope",text))
     
     #reads each part of the file into the data list
     #GPS returns a list with Status part and data part
     data_list[["GPS"]] <- read_VUZIX_GPS(text,GPS_idx)
     #data["compass"] = read_VUZIX_data(text,compass_idx)
     #data["gyroscope"] = read_VUZIX_data(text,gyroscope_idx)
     #data["accelerometer"] = read_VUZIX_data(text,accelerometer_idx)
     
     #returns the list
     return(data_list)
}

read_VUZIX_data <- function(text,indexes){
     return(read.table(textConnection(text[indexes]),sep=","))
}

read_VUZIX_GPS <- function(text,indexes){
     new_text = text[indexes]
     #gets the lines of "STATUS CHANGED"
     status_idx <- grepl("STATUS CHANGE",new_text)
     #Creates a table of only statuses
     status_tab <- read.table(textConnection(new_text[status_idx]),sep=",")
     #table With GPS information
     data_tab <- read.table(textConnection(new_text[!status_idx]),sep=",",dec=".",colClasses=c("numeric","character","character","character","numeric","numeric","character"))
     return(list(data=data_tab,status=status_tab))
     
}