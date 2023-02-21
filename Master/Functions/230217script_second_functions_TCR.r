mysinglecell_metadata_for_alphadiversity <- function(seurat_object){
    select(seurat_object@meta.data, !starts_with('TCR')) %>% select(!starts_with('BCR')) %>% select(-orig.ident, -nCount_RNA, -nFeature_RNA, -RNA_snn_res.0.5, -seurat_clusters) %>% names() -> group
    return(group)
}

alphaDiversity_TCR<- function(seurat_object, group = "Sample", q = 'all'){
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(all_of(group), TCR_TRB_raw_clonotype_id)
    diversity <- alphaDiversity(metadata, group=group, min_q=0, max_q=4, step_q=1, nboot=100, clone="TCR_TRB_raw_clonotype_id")
    if (q == 'all'){
    p1 <- plot(diversity)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
        }
    else if(q != 'all'){
    p1 <- plot(diversity, q)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
    }
    return(p1)

}
