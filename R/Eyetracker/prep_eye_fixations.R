#' The complication is that we are delaing with two different types of resolutions
#' Unity resolution and screen resolution - unity one will lead to skewed image, whereas screen
#' will lead to different numbers to be output by the eyetracker
prep_eye_fixations = function(dt, disp_resolution, unity_class){
  
  DEFAULT_RESOLUTION = list(width = 1920, height = 1080)
  ls = list()
  ls$changed = F
  result = copy(dt)
  if(is.null(disp_resolution)) disp_resolution = DEFAULT_RESOLUTION
  
  if(check_if_preprocessed(dt)){
    ls$result = result
    return(ls)
  }
  
  # -------- Reversing Y fixations  -------
  result[, pos_y := disp_resolution$height - pos_y]
  
  # -------- Fixing display resolution -------
  # It basically recalculates it to the 1920 x 1080
  if(disp_resolution$width != DEFAULT_RESOLUTION$width || disp_resolution$height != DEFAULT_RESOLUTION$height){
    SmartPrint(c("fixing display resolution in", unity_class$id))
    result[, pos_x := round(pos_x/disp_resolution$width * DEFAULT_RESOLUTION$width)]
    result[, pos_y := round(pos_y/disp_resolution$height * DEFAULT_RESOLUTION$height)]
  }

  # -------- Fixing unity resolution --------
  if(!is.null(unity_class)){
    unity_screen = unity_class$screen_size()
    unity_screen = recompute_screen_size(unity_screen, DEFAULT_RESOLUTION)
    min_x = (DEFAULT_RESOLUTION$width - unity_screen$width)/2
  }
  
  # needs to recompute the issue of smaller resolutions used in late participants
  if(unity_screen$width != DEFAULT_RESOLUTION$width || unity_screen$height != DEFAULT_RESOLUTION$height){
    SmartPrint(c("fixing resolution in", unity_class$id))
    result = result[, pos_x := pos_x - min_x ]
    result[, pos_x := round(pos_x/unity_screen$width * DEFAULT_RESOLUTION$width)]
    result[, pos_y := round(pos_y/unity_screen$height * DEFAULT_RESOLUTION$height)]
  }
  
  # -------- removing out of the screen fixations --------
  result = result[pos_x < DEFAULT_RESOLUTION$width & pos_y < DEFAULT_RESOLUTION$height, ]
  result = result[pos_x > 0 & pos_y > 0, ]
  
  if ("no_idea_1" %in% colnames(result))result[, no_idea_1 := NULL]
  
  ls$changed = T
  ls$result = result
  return(ls)
}

check_if_preprocessed = function(dt){
  if ("no_idea_1" %in% colnames(dt)) return(FALSE)
  return(TRUE)
}

#' This is just weird but works. What happens is that if the resolution doesn't match it fits the dimension
#' and then spreads the other dimenstion. So in case of 4:3 (1024*768) the height it matched to the resolution 
#' of the screen (usually 1080) and the width is recomputed
recompute_screen_size = function(screen_size, DEFAULT_RESOLUTION){
  screen_size$width = (DEFAULT_RESOLUTION$height/screen_size$height)*screen_size$width
  screen_size$height = DEFAULT_RESOLUTION$height
  return(screen_size)
}