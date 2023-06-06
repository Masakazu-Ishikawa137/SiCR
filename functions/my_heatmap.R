

my_heatmap <- function(
    
  data,
  x,
  y,
  fill,
  round=1,
  label_size=4,
  legend_position="right",
  low_color="white",
  high_color="grey40",
  color_limit=NA # if NA, scale_fill_gradient set it as max value automatically.
  
) {
  
  if (isTRUE(legend_position)) {
    legend_position <- "right"
  } else if (isFALSE(legend_position)) {
    legend_position <- "none"
  }
  
  # When using geom_raster, the colors become strange when saving with ggsave (possibly related to the Mac viewer?), so I use geom_tile instead.
  # When using hrbrthemes::theme_ipsum(), ggsave doesn't work properly.
  g <- ggplot(data, aes(x = get(x), y = get(y))) +
    geom_tile(aes(fill = get(fill))) +
    geom_text(size=label_size, aes(label=round(get(fill), round))) +
    scale_fill_gradient(low = low_color, high = high_color, na.value=high_color, limits=c(0, color_limit)) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    theme(
      axis.ticks = element_line(colour = "grey80"),
      axis.text.x  = element_text(angle = 90, hjust = 1, vjust=0.5),
      legend.position = legend_position
    ) +
    labs(x = element_blank(), y = element_blank(), fill = fill)
  
  return(g)

}