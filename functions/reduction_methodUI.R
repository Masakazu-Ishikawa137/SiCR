reduction_methodUI <- function(ns){
    radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap')
}
