require(png)
require(grid)
require(ggplot2)

make_path_image <- function (positions,locatioin,img_location,position_table){

     img <- readPNG(img_location)
     
     g <- rasterGrob(img, width=unit(1,"native"), height=unit(1,"native"))
     g <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"))
     
     ggplot(position_table, aes(Position.x, Position.z))  +
          
          annotation_custom(g, -1058, 1442, -842, 908)+
          scale_x_continuous(limits = c(-1058, 1442)) +
          scale_y_continuous(limits = c(-842,908)) + geom_path(size = 1)
     
}

calculate_cululative_time <- function(){
}