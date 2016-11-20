prep_eye_fixations = function(dt, unity_class){
  
  DEFAULT_RESOLUTION = list(width = 1920, height = 1080)
  ls = list()
  ls$changed = F
  result = copy(dt)
  #removing out of the screen fixations
  if(!is.null(unity_class)){
    screen_size = unity_class$screen_size()
    screen_size = recompute_screen_size(screen_size, DEFAULT_RESOLUTION)
    min_x = (DEFAULT_RESOLUTION$width - screen_size$width)/2
  } else {
    screen_size = DEFAULT_RESOLUTION
    min_x = 0
  }
  if (result[pos_x > screen_size$width, .N] > 0 || result[pos_y > screen_size$height, .N] > 0){
    result = result[, pos_x := pos_x - min_x ]
    result = result[pos_x < screen_size$width & pos_y < screen_size$height, ]
    result = result[pos_x > 0 & pos_y > 0, ]
    result[, pos_y := screen_size$height - pos_y]
    ls$changed = T
  }
  #needs to recompute the issue of smaller resolutions used in late participants
  if(screen_size$width != DEFAULT_RESOLUTION$width || screen_size$height != DEFAULT_RESOLUTION$height){
    SmartPrint(c("fixing resolution in", unity_class$id))
    result[, pos_x := round(pos_x/screen_size$width * DEFAULT_RESOLUTION$width)]
    result[, pos_y := round(pos_y/screen_size$height * DEFAULT_RESOLUTION$height)]
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

#' This is just weird but works. What happens is that if the resolution doesn't match it fits the dimension
#' and then spreads the other dimenstion. So in case of 4:3 (1024*768) the height it matched to the resolution 
#' of the screen (usually 1080) and the width is recomputed
recompute_screen_size = function(screen_size, DEFAULT_RESOLUTION){
  screen_size$width = (DEFAULT_RESOLUTION$height/screen_size$height)*screen_size$width
  screen_size$height = DEFAULT_RESOLUTION$height
  return(screen_size)
}