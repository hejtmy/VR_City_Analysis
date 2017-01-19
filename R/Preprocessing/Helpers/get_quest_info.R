get_quest_info = function(quest_log){
  ls = list()
  ls[["name"]] = names(quest_log)
  
  #gets all the letters and numbers until the dash(-) symbol
  #first is E in VR experiments, second the quest index and then the a/b version
  id_pattern = "(.*?)-"
  id_part = str_match(ls[["name"]],id_pattern)[2]
  if(is.na(id_part)) stop("not clear quest log naming")
  #checks for MRI/Eyetracker
  MRILog = if(is.na(str_match(id_part, "[AB]")[1])) FALSE else TRUE
  if (MRILog){
    #boreing complicated stuff because the naming of quests conventions don't make sense
    ls$id = as.numeric(str_match(id_part, "[AB](\\d+)")[2])
    if(!is.na(str_match(id_part, "[A]")[1])) ls[["id"]] = ls[["id"]]*2
    if(!is.na(str_match(id_part, "[B]")[1])) ls[["id"]] = ls[["id"]]*2-1
  } else {
    ls$id = as.numeric(str_match(id_part, "[E](\\d+)")[2])
  }
  if(is.null(ls$id)) stop ("No appropriate id")
  #getting type from the name of the log 
  #MRI has B for trials with directions and A for trials
  #Eyetracker has a for learning trials and "b" for trials
  learn = c("a", "B")
  trial = c("b", "A")
  type_pattern = "[aAbB]"
  if(is.na(str_match(id_part, type_pattern)[1])) stop("not clear quest log naming")
  type_string = str_match(id_part, type_pattern)[1]
  type = NA
  if (type_string %in% learn) type = "learn"
  if (type_string %in% trial) type = "trial"
  ls[["type"]] = type
  return(ls)
}
