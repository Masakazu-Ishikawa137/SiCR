reduction_methodUI <- function(ns){
    radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap')
}

group_byUI <- function(ns){
    radioButtons(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}

split_byUI <- function(ns){
    radioButtons(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}

point_sizeUI <- function(ns){
    sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01)
}

label_sizeUI <- function(ns){
    sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 5, step = 1)
}

legend_placeUI <- function(ns){
    radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right")
}

plot_sizeUI <- function(ns){
    tagList(
        sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
        sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100)
    )
}

gene_textUI <- function(ns){
       textInput(ns("gene"), "Enter feature (gene) names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19")
}

feature_columnUI <- function(ns){
    sliderInput(ns('feature_column'), label = 'Number of columns', min = 1, max = 10, value = 1)
}

focus_groupUI <- function(ns){
    checkboxGroupInput(ns("focus_group"), "Show top 20 in this group", choices = "", inline = TRUE)
}

highlight_sizeUI <- function(ns){
    sliderInput(ns("highlight_size"), "Size of highlight", min = 0.01, max = 10,  value = 0.1, step = 0.01)
}