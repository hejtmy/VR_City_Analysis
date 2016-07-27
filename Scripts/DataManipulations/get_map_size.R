get_map_size = function(quest_set, trial_sets, quest = NULL){
  ls = list()
  if (is.null(quest)) quest = quest(quest_set, trial_sets, 1)
  quest_line = filter(quest_set, name == quest$name)
  terrain_info = trial_sets[[quest_line$id_of_set]]$experiment_log$terrain
  size = text_to_vector3(terrain_info$Size)
  pivot = text_to_vector3(terrain_info$Pivot)
  ls[["x"]] = c(pivot[1],pivot[1] + size[1])
  ls[["y"]] = c(pivot[3],pivot[3] + size[3])
  return(ls)
}