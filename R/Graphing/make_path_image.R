make_path_image = function (img_location, position_table, map_size){
  
  img = readPNG(img_location)
  
  g = rasterGrob(img, width=unit(1, "native"), height=unit(1, "native"))
  g = rasterGrob(img, width=unit(1, "npc"), height=unit(1, "npc"))
  
  plot = ggplot(position_table, aes(Position.x, Position.z))  +
    annotation_custom(g, xmin = map_size$x[1], xmax = map_size$x[2], ymin = map_size$y[1], ymax = map_size$y[2])+
    scale_x_continuous(limits = map_size$x) +
    scale_y_continuous(limits = map_size$y)
  
  #if we want to draw some part of the track special colours
  if("special" %in% colnames(position_table)){
    plot = plot + geom_path(size = 1, aes(colour = special), show.legend = T)
  } else {
    plot = plot + geom_path(size = 1)
  }
  return(plot)
}
