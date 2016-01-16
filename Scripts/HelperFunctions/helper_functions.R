require(stringr)
open_player_log <- function(dir = "", patient_code ="", date_time = ""){
     
     ptr = paste(patient_code, "_player_", date_time, "*.txt$", sep="")
     
     #needs to check if we got only one file out
     log = list.files(dir, pattern = ptr ,full.names = T)[1]
     
     #reads into a text file at first
     text = readLines(log,warn=F)
     #finds the header start
     idxTop <- which(grepl('\\*\\*\\*\\*\\*',text))
     #finds the header bottom
     idxBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
     #potentially returns the header as well in a list
     #todo
     
     #reads the data without the header file
     pos_tab <- read.table(log, header=T,sep=";",dec=".", skip=idxBottom, stringsAsFactors=F)
     
     #deletes the last column - it's there for the easier logging from unity
     pos_tab[,length(names(pos_tab))]=NULL
     
     pos_tab <- vector3_to_columns(pos_tab,"Position")
     return(pos_tab)
     
}

open_experiment_log <- function(dir = "", patient_code ="", datetime = ""){
     
     ls = list()
     ptr = paste(patient_code, "_experiment_", datetime, "*.txt$", sep="")
     
     #needs to check if we got only one file out
     log = list.files(dir, pattern = ptr,full.names = T)[1]
     
     #reads into a text file at first
     text = readLines(log,warn=F)
     #finds the header start
     idxHeaderTop <- which(grepl('\\*\\*\\*\\*\\*',text))
     #finds the header bottom
     idxHeaderBottom <- which(grepl('\\-\\-\\-\\-\\-',text))
     #potentially returns the header as well in a list
     ls[["header"]] <- into_list(text[(idxHeaderTop+1):(idxHeaderBottom-1)])
     
     #todo
     idxTerrainTop <- which(grepl('\\*\\*\\*Terrain information\\*\\*\\*',text))
     idxTerrainBottom <- which(grepl('\\-\\-\\-Terrain information\\-\\-\\-',text))
     ls[["terrain"]]  <- into_list(text[(idxTerrainTop+1):(idxTerrainBottom-1)])
     return(ls)     
}

into_list <- function(text =""){
     ls <- list()
     for (info in text) {
          split <- str_split(info, pattern = ":")
          code <- split[[1]][1]
          value <- str_extract_all(split[[1]][2],"\\*[^()]+\\*")[[1]][1]
          value <- substring(value,2,nchar(value)-1)
          ls[[code]] <- value
     }
     return(ls)
}

vector3_to_columns <- function(tab, column_name){
     
     xyz <- c("x","y","z")
     
     splitted <- strsplit(substring(tab[,column_name],2,nchar(tab[,column_name])-1),",")
     
     #turns the Vector3 into lists of 3 values
     i = 1
     for (letter in xyz){
          new_name <- paste(column_name,letter,sep=".")
          tab[,new_name] <- as.numeric(sapply(splitted,"[",i))
          i<-i+1
     }
     return(tab)

}