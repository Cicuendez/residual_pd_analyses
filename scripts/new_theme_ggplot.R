new_theme <- function(base_size = 12) {
  theme(
    axis.line =         element_blank(),
    axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        element_line(colour = "black", size = 0.2),
    axis.title.x =      element_text(size = base_size, vjust = 1),
    axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
    
    legend.background = element_rect(colour=NA), 
    legend.key =        element_rect(colour = NA),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       element_text(size = base_size * 0.8),
    legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "bottom",
    legend.direction =  "horizontal",
    
    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
    panel.spacing =      unit(0.25, "lines"),
    
    strip.background =  element_rect(fill = "grey80", colour = "grey50"),
    # strip.background =  element_rect(fill = "orange", colour = "grey50"), 
    strip.text.x =      element_text(size = base_size * 0.8),
    strip.text.y =      element_text(size = base_size * 0.8, angle = -90),
    
    plot.background =   element_rect(colour = NA),
    plot.title =        element_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  )
}
