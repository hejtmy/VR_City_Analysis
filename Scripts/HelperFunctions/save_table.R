save_table = function(path, table){
  if(file.exists(path)){
    #does something
  }
  SmartPrint(c("Saving table as", path))
  write.table(table, path, sep=";", dec=".", quote=F, row.names = F)
}