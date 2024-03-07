split_byUI <- function(ns){
    radioButtons(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}
