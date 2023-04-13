
my_barplot <- function(
    
  data,
  x,
  y,
  fill,
  position="dodge",
  legend_position="right",
  flip=FALSE
  
) {
  
  g <- ggplot(data)+
    geom_col(aes(x = get(x), y = get(y), fill = get(fill)), position = position) +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    labs(fill = fill, y = y, x = x) +
    ggsci::scale_fill_nejm()
  
  if(flip) {
    g <- g +
      coord_flip() +
      theme(legend.position = legend_position)
  } else {
    g <- g +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1), # angle=90の場合vjust=0.5
            legend.position = legend_position)
  }
  
  return(g)
  
}