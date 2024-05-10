update_group_by <- function(session, myReactives) {
  print('update_group_by')
  minus_column <- c("orig.ident", "nCount_RNA", "nFeature_RNA", "barcode", "percent.mt", "RNA_snn_res.0.5")
  metadatas <- myReactives$seurat_object@meta.data %>%
    select(-minus_column) %>%
    select(!starts_with("TCR")) %>%
    select(!starts_with("BCR"))
  metadata_cols <- names(metadatas)
  group_cols <- list()
  for (col in metadata_cols) {
    group_cols[[col]] <- col
  }
  updateRadioButtons(session, "group_by", choices = group_cols, selected = "seurat_clusters")
}

# group_col <- function(myReactives){
#   group_cols <- list("seurat_clusters" = "seurat_clusters", "sample" = "sample")
#   minus_column <- c('barcode','is_cell','contig_id','high_confidence','length','chain','v_gene','d_gene','j_gene','c_gene','full_length','productive','fwr1','fwr1_nt','cdr1','cdr1_nt','fwr2','fwr2_nt','cdr2','cdr2_nt','fwr3','fwr3_nt','cdr3','cdr3_nt','fwr4','fwr4_nt','reads','umis','raw_clonotype_id','raw_consensus_id','exact_subclonotype_id')
#   df <-
#     df <- csv %>% select(-minus_column) %>% distinct(sample, .keep_all=TRUE) %>% select(sample, everything())


#   meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
#   for(col in meta_data_cols) {
#     group_cols[[col]] <- col
#   }
#   return(group_cols)
# }

# new_group_col <- function(myReactives){
#   if(is.null(myReactives$group_cols)){
#     myReactives$group_cols <- list()
#   }
#   myReactives$group_cols <- list(myReactives$group_cols, "seurat_clusters" = "seurat_clusters", "sample" = "sample")
#   meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
#   for(col in meta_data_cols) {
#     myReactives$group_cols[[col]] <- col
#   }
#   return(myReactives)
# }


#       if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         group_cols <- group_col(myReactives)
#         if ("TCR" %in% names(myReactives$seurat_object@meta.data)) {
#           group_cols[["TCR"]] <- "TCR"
#           group_cols[["TCR_count_per_sample"]] <- "TCR_count_per_sample"
#           group_cols[["TCR_expand"]] <- "TCR_expand"
#         }

#         if ("BCR" %in% names(myReactives$seurat_object@meta.data)) {
#           group_cols[["BCR"]] <- "BCR"
#           group_cols[["BCR_count_per_sample"]] <- "BCR_count_per_sample"
#           group_cols[["BCR_expand"]] <- "BCR_expand"
#         }
