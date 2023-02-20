#source('230217script_first_functions_GEX.r')
#source('230217script_first_functions_TCR.r')
#source('230217script_first_functions_BCR.r')


GEX <- function(h5){
    seurat_object <- mysinglecell_h5_to_umap(h5)
#    seurat_object <- mysinglecell_celltyping_normal_normalization2(seurat_object)
    return (seurat_object)
}

TCR <- function(seurat_object, tcr){
    mysinglecell_add_metadata(seurat_object, tcr) -> seurat_object
    myTCR_dataframe(tcr) -> tcr_dataframe
    myconcat_seumeta_tcr(seurat_object, tcr_dataframe) -> seurat_object    
    return (seurat_object)
}

BCR <- function(seurat_object, bcr){
     mysinglecell_add_metadata(seurat_object, bcr) -> seurat_object
     myBCR_dataframe(bcr) -> bcr_dataframe
     myconcat_seumeta_bcr(seurat_object, bcr_dataframe) -> seurat_object    
     return (seurat_object)
}
