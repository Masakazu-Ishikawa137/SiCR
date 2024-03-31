group_col <- function(myReactives){
  group_cols <- list("seurat_clusters" = "seurat_clusters", "sample" = "sample")
  meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
  for(col in meta_data_cols) {
    group_cols[[col]] <- col
  }
  return(group_cols)
}