create_tracking_df <- function(seurat_object) {
    if(!is.null(seurat_object)) {
        tracking_df <- seurat_object@meta.data %>% 
            filter(!is.na(TCR_TRB_v_gene)) %>% 
            group_by(sample, TCR_TRB_v_gene) %>% 
            summarise(count = n()) %>% 
            ungroup() %>% 
            group_by(sample) %>% 
            mutate(total_count = sum(count)) %>% 
            ungroup() %>% 
            mutate(proportion = count / total_count)
        return(tracking_df)
    }
    return(NULL)
}