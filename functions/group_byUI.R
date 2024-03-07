group_byUI <- function(ns){
    radioButtons(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}
