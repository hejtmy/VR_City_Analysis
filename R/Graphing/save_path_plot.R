save_path_plot = function(plot, name, FOLDER){
  mypath = paste(FOLDER, name, sep = "/", collapse = "")
  file = png(mypath, width = 1200, height = 800, units = "px")
  plot(plot)
  dev.off()
}
