add_pointing_arrows = function(plt, pointing_df, start_stop){
  ARROW_LENGTH = 100
  ARR_DEF = arrow(length = unit(0.25, "cm"))
  
  start_pos = start_stop$start
  stop_pos = start_stop$finish
  
  segment_start_chosen = add_arrow(start_pos, pointing_df[1, 'chosen_angle'], ARROW_LENGTH)
  segment_start_target = add_arrow(start_pos, pointing_df[1, 'target_angle'], ARROW_LENGTH)
  segment_stop_chosen = add_arrow(stop_pos, pointing_df[2, 'chosen_angle'], ARROW_LENGTH)
  segment_stop_target = add_arrow(stop_pos, pointing_df[2, 'target_angle'], ARROW_LENGTH)
  segments = as.data.frame(rbind(segment_start_chosen, segment_start_target, segment_stop_chosen, segment_stop_target))
  colnames(segments) = c('x', 'y', 'xend', 'yend')
  segments$type = rep(c("chosen", "target"), 2)
  
  plt = plt + geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend, color = type), size = 1, arrow = ARR_DEF)
}

add_arrow = function(position, angle, len){
  return(c(position, position + positions_from_angle(angle) * len))
}