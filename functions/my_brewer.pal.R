my_brewer.pal <- function(required_n, palette){
  
  max_n <- brewer.pal.info$maxcolors[which(rownames(brewer.pal.info) == palette)]
  colors <- brewer.pal(max_n, palette)
  
  if(required_n > length(colors)){
    rep_n <- ceiling(required_n / length(colors))
    colors <- rep(colors, rep_n)
  }
  
  return(colors)
  
}