mysinglecell_metadata_for_UMAP <- function(seurat_object){
    select(seurat_object@meta.data, !starts_with('TCR')) %>% select(!starts_with('BCR')) %>% select(-orig.ident, -nCount_RNA, -nFeature_RNA, -RNA_snn_res.0.5) %>% names() -> group
    return(group)
}