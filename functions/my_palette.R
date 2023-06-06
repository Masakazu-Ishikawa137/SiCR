my_palette <- function(required_n, palette){
  
  if(palette == "Default" | palette == "default"){
    colors <- scales::hue_pal()(required_n)
    return(colors)
  }
  
  if(palette == "nejm"){
    colors <- ggsci::pal_nejm("default")(8)  
  } else { # palette should be RColorBrewer
    max_n <- brewer.pal.info$maxcolors[which(rownames(brewer.pal.info) == palette)]
    colors <- brewer.pal(max_n, palette)
  }
  
  if(required_n > length(colors)){
    rep_n <- ceiling(required_n / length(colors))
    colors <- rep(colors, rep_n)
  }
  
  return(colors)
  
}