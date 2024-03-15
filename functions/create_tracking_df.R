# create_tracking_df <- function(seurat_object) {
#     if(!is.null(seurat_object)) {
#         tracking_df <- seurat_object@meta.data %>% 
#             filter(!is.na(TCR_TRB_v_gene)) %>% 
#             group_by(sample, TCR_TRB_v_gene) %>% 
#             summarise(count = n()) %>% 
#             ungroup() %>% 
#             group_by(sample) %>% 
#             mutate(total_count = sum(count)) %>% 
#             ungroup() %>% 
#             mutate(proportion = count / total_count)

#         df_complete <- df %>%
#             complete(sample, TCR_TRB_v_gene, fill = list(count = 0, total_count = 0, proportion = 0))

#         df_complete <- df_complete %>%
#             group_by(sample) %>%
#             mutate(total_count = sum(count)) %>%
#             ungroup()

#         df_complete <- df_complete %>%
#             mutate(proportion = count / total_count)


#         return(df_complete)
#     }
#     return(NULL)
# }

create_tracking_df <- function(seurat_object) {
  if(!is.null(seurat_object)) {
    tracking_df <- seurat_object@meta.data %>% 
      filter(!is.na(TCR_TRB_v_gene)) %>% 
      group_by(sample, TCR_TRB_v_gene) %>% 
      summarise(count = n(), .groups = 'drop') %>% 
      mutate(total_count = sum(count), proportion = count / total_count)
    
    # tidyr::completeを使用してエラーを避ける
    tracking_df_complete <- tracking_df %>%
      tidyr::complete(sample, TCR_TRB_v_gene, fill = list(count = 0, total_count = 0, proportion = 0)) %>%
      group_by(sample) %>%
      mutate(total_count = sum(count)) %>%
      ungroup() %>%
      mutate(proportion = count / total_count)

    return(tracking_df_complete)
  }
  return(NULL)
}