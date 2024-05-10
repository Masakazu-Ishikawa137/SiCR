update_filter_slide <- function(session, myReactives) {
    print("update_filter_slide")
    seurat_object <- myReactives$seurat_object
    updateSliderInput(session, "slider_nCount_RNA", min = min(seurat_object@meta.data$nCount_RNA), max = max(seurat_object@meta.data$nCount_RNA), value = c(min(seurat_object@meta.data$nCount_RNA), max(seurat_object@meta.data$nCount_RNA)))
    updateSliderInput(session, "slider_nFeatures_RNA", min = min(seurat_object@meta.data$nFeature_RNA), max = max(seurat_object@meta.data$nFeature_RNA), value = c(min(seurat_object@meta.data$nFeature_RNA), max(seurat_object@meta.data$nFeature_RNA)))
    updateSliderInput(session, "slider_percent_mt", min = 0, max = floor(max(seurat_object@meta.data$percent.mt)) + 1, value = max(seurat_object@meta.data$percent.mt))
    updateCheckboxGroupInput(session, "subsetting", choices = sort(unique(seurat_object@meta.data$seurat_clusters)))
}
