make_path_image = function (img_location, position_table, map_size, special_points = NULL){
  
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
  if (!is.null(special_points)){
    plot = AddPointsToPlot(plot, special_points)
  }
  return(plot)
}
AddPointsToPlot = function(plot, ls){
  list_names = names(ls)
  data_table = data.frame(point.x = numeric(0),point.y = numeric(0), point.name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    data_table[i,1] = ls[[i]][1]
    data_table[i,2] = ls[[i]][2]
    data_table[i,3] = list_names[i]
  }
  plot = plot + 
    geom_point(data = data_table, aes(point.x, point.y, color = point.name), size = 4)
  return(plot)
}