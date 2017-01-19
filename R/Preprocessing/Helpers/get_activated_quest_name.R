#helper function to figure out the name of the activated quest as is saved in the steps
#list in the scenario quest
get_activated_quest_name = function(string = ""){
  #The name of the quest is between square brackets - [quest name]
  name = str_extract_all(string,"\\[(.*?)\\]")[[1]][1]
  #removing the square brackets
  name = substring(name, 2, nchar(name) - 1)
  return(name)
}