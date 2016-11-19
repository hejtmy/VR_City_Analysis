prep_eye_fixations = function(dt, unity_class){
  ls = list()
  ls$changed = F
  result = copy(dt)
  #removing out of the screen fixations
  if(!is.null(unity_class)){
    screen_size = unity_class$screen_size()
  } else {
    screen_size = list(width = 1920, height = 1080)
  }
  if (result[pos_x > screen_size$width, .N] > 0 || result[pos_y > screen_size$height, .N] >0){
    result = result[pos_x < screen_size$width & pos_y < screen_size$height, ]
    result = result[pos_x > 0 & pos_y > 0, ]
    result[, pos_y := screen_size$height - pos_y]
    ls$changed = T
  }
  if ("no_idea_1" %in% colnames(result)){
    result[, no_idea_1 := NULL]
    ls$changed = T
  }
  
  #reverting fixations in the Y axis
  ls$result = result
  return(ls)
}