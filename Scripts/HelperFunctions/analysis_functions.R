require(png)
require(grid)
require(ggplot2)

make_path_image <- function (img_location,position_table, special = NULL){

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
       plot+ geom_path(size = 1,aes(colour = special))
     }else {
       plot + geom_path(size = 1)
     }
}

calculate_cululative_time <- function(){
}