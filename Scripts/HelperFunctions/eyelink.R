source()

read_eyelink = function(path = ""){
  ls=list()
  
  if(!file.exists(path)){
    print("Could not find the file for given eyelink file")
    print(path)
    return(NULL)
  }

  text = readLines(path,warn=F)
  
  #gets all the tabular information
  idxTable = which(grepl('^\\d+\\s+\\d+(?:\\.\\d)?\\s+\\d+(?:\\.\\d)?.*$',text))
  table = read.table(textConnection(text[idxTable]), header=T, sep="",dec=".", stringsAsFactors=F)
}