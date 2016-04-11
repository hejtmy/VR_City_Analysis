require(png)
require(grid)
require(ggplot2)

make_path_image <- function (img_location,position_table, special = NULL, special_points = NULL){

     img <- readPNG(img_location)
     
     g <- rasterGrob(img, width=unit(1,"native"), height=unit(1,"native"))
     g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
     
     plot = ggplot(position_table, aes(Position.x, Position.z))  +
       annotation_custom(g, -1058, 1442, -842, 908)+
       scale_x_continuous(limits = c(-1058, 1442)) +
       scale_y_continuous(limits = c(-842,908))
     #if we want to draw some part of the track special colours
     if (!is.null(special)){
       position_table = position_table[, special:= is_between(Time,special[1],special[2])]
       plot = plot + geom_path(size = 1,aes(colour = special))
     }else {
       plot = plot + geom_path(size = 1)
     }
     if (!is.null(special_points)){
       plot = AddPointsToPlot(plot,special_points)
     }
     return(plot)
}

AddPointsToPlot = function(plot, ls){
  list_names = names(ls)
  data_table = data.frame(point.x=numeric(0),point.y=numeric(0), point.name=character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    data_table[i,1] = ls[[i]][1]
    data_table[i,2] = ls[[i]][2]
    data_table[i,3] = list_names[i]
  }
  plot = plot + geom_point(data = data_table, aes(point.x,point.y),size = 4, color = "blue") + geom_text(data = data_table, aes(point.x, point.y,label=point.name))
  return(plot)
}

calculate_cululative_time <- function(){
}