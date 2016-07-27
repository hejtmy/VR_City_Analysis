prep_eye_fixations = function(dt, unity_class){
  ls = list()

  result = copy(dt)
  #removing out of the screen fixations
  if(!is.null(unity_class)){
    screen_size = unity_class$screen_size()
  } else {
    screen_size = list(width = 1920, height = 1080)
  }
  #reverting fixations in the Y axis
  result[, pos_y := abs(pos_y - screen_size$height)]
  result = result[pos_x < screen_size$width & pos_y < screen_size$height, ]
  ls$changed = T
  ls$result = result
  return(ls)
}