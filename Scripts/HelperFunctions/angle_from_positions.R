#' 
#' Needs to be in order FROM -> TO, otherwise can provide weird values

angle_from_positions = function(pos_from, pos_to){
  
  #' Zero vector is the vector given by the unity for calculation rotation of oabjects. 
  #' In 3D unity (different in UNREAL!) are exes X horizontal left/right, Y vertical up/down, and Z plane horizontal up/down
  #' The zero vector is vector when GameObject only changes its Z position, therefore if we calculate position as [X Z], 
  #' the normalised zero vector shoudl be 0 on X and 1 on Z - [0, 1]
  
  ZERO_VECTOR =  c(0, 1)
  target_vector = pos_to - pos_from
  theta = acos(sum(ZERO_VECTOR * target_vector) / (sqrt(sum(ZERO_VECTOR * ZERO_VECTOR)) * (sqrt(sum(target_vector * target_vector)))))
  angle = radian_to_angle(theta)
  return(angle)
}

radian_to_angle = function(radian){
  return(radian/pi * 180)
}