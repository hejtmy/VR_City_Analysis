require('R6')
source(paste(getwd(),"Scripts","HelperFunctions/preprocess_functions.R",sep="/"))
source(paste(getwd(),"scripts","HelperFunctions/analysis_functions.R",sep="/"))

VUZIX_analysis <- R6Class(     "VUZIX_analysis",
     #define variables
      public = list(
        #basic definitions
        id = NULL,
        #data directory
        dir = NULL,
        log_file = NULL,
        log_name = NULL,
        #loaded tables and lists
        #GPS is a list of Status changes and data information of lattitude/longitude
        GPS = NULL, #list
        accelometer = NULL, #data.frame
        gyroscope = NULL, #data.frame
        compass = NULL, #data.frame

        initialize = function(dir = getwd(), id="", log_name=""){
          if(nargs() > 1) {
            self$dir = paste(dir,"Data",id,'VUZIX',sep="/")
            self$log_name <- log_name
            self$log_file <- paste(self$dir,log_name,sep="/",collapse=NULL)
          }
          if(nargs() >= 2) {
            data_list <- open_VUZIX_log(self$log_file)
            self$GPS <- data_list$GPS
          }
        }
     ),
     
     private = list(
       
       is_valid = function(){
         
       }
     )
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