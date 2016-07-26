prep_eye_fixations = function(dt, unity_class){
  ls = list()
  #removing out of the screen fixations
  if(!is.null(unity_class)){
    screen_size = unity_class$screen_size
    
  }
  #reverting fixations
  ls$changed = T
  ls$result = dt
  return(ls)
}