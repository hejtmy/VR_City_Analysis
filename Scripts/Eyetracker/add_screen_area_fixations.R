#' Returns fixations with added information about to which evnet each fixation belongs
#' 
#' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
#' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
#' Even in correct table there are two events that are off, as those are differences between two different sets administered
#' 
#' @param dt_fixations fixations data.table. Needs to have posX and posY
#' @param areas vector of lists with name and vector of points
#' 

height = 1080
width = 1920
quater = function(num){return(num/4)}
third = function(num){return(num/3)}
around = function(middle, deviation){return(c(middle - deviation, middle + deviation))}

map_area = list(name = "map", x = c(0, quater(width)), 
                              y = c(0, quater(height)))

center_area = list(name = "center", x = around(width/2, quater(width)/2), 
                                    y = around(height/2, quater(height)/2))

control_area = list(name = "control", x = c(0, quater(width)),
                                      y = c(height, height - quater(height)))

quest_area = list(name = "quest", x = c(width, width - quater(width)),
                                  y = c(height, height - quater(height)/2))

areas = list(map_area, center_area, control_area)

add_screen_area_fixations = function(dt_fixations, areas){
  dt_fixations[, area := ""]
  for (ar in areas){
    dt_fixations[is_between(pos_x, ar$x[1], ar$x[2]) & is_between(pos_y, ar$y[1], ar$y[2]), area:= ar$name]
  }
}