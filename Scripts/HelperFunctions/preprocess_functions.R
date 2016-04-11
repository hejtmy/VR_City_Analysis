require(stringr)
require(plyr)

#the header files ar writen in this format
## PROPERTY NAME: *VALUE*
## PROPERTY NAME 2: *VALUE 2*
#this function sorts that out into the list/dictionary of keys and values
into_list <- function(text =""){
     ls <- list()
     #fir each line
     for (info in text) {
          #finds the PROEPRTY NAME
          split <- str_split(info, pattern = ":",n=2)
          #extract the value from the str_split list (this is a weird line but
          #strsplit creates a list of lists so we need to do this
          code <- split[[1]][1]
          #extracting the VALUE from the second part of the list
          value <- str_extract_all(split[[1]][2],"\\*(.*?)\\*")[[1]][1]
          #removing the *
          value <- substring(value,2,nchar(value)-1)
          #saving into the list 
          ls[[code]] <- value
     }
     return(ls)
}
#turns vector columns in string "(x, y, z)" into three columns(Position.x, Position.y, Position.z) and returns the table
vector3_to_columns <- function(tab, column_name){
     
     xyz <- c("x","y","z")
     splitted <- strsplit(substring(tab[,get(column_name)],2,nchar(tab[,get(column_name)])-1),",")
     #turns the Vector3 into lists of 3 values
     i = 1
     for (letter in xyz){
      new_name <- paste(column_name,letter,sep=".")
      tab[,(new_name):=as.numeric(sapply(splitted,"[",i))]
      i<-i+1
     }
     return(tab)
}

text_to_vector3 = function(text){
  splitted = strsplit(substring(text,2,nchar(text)-1),",")
  #final = as.numeric(splitted)
  final = sapply(splitted, as.numeric)
  return(final)
}
##Helper for escaping characters in quest names
escapeRegex <- function(string){
     
     return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string))
}

#calculates the distance walked between each two points of the position table and returns the table
AddDistanceWalked = function(position_table){
  distances = numeric(0)
  for (i in 2:nrow(position_table)){
    position_table[c(i-1,i),distance:= EuclidDistanceColumns(.(Position.x,Position.z)[1], .(Position.x,Position.z)[2])]
    #distances = c(distances,EuclidDistance(position_table[i,list(Position.x,Position.z)],position_table[i-1,list(Position.x,Position.z)]))
  }
  position_table[,cumulative_distance:=cumsum(distance)]
  return(position_table)
}