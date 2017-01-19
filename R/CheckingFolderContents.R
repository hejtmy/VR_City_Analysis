library(data.table)
CheckingFiles = function(path){
  subject_file_data = list()
  subject_paths = list.dirs(path,recursive = F)
  #for each subject
  for(path in subject_paths){
    subject_info = list()
    subject_files = list.files(path,full.names = T, recursive = T)
    #get time for each
    for (i in 1:length(subject_files)){
      subject_info[[i]] = file.info(subject_files[[i]])
      subject_info[[i]]$filename = basename(subject_files[[i]])
    }
    subject_file_data[[basename(path)]] = rbindlist(subject_info)
  }
  table = rbindlist(subject_file_data, idcol = T)
  return(table)
}
path = "/"
a = CheckingFiles(path)
a[,day:= format(mtime,"%A")]
