        # update_group_by <- function(myReactives, session){   
        #   observe({
        #     if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        #       meta_data_cols <- names(myReactives$seurat_object@meta.data)
        #       exclude_cols <- c("orig.ident", "nCount_RNA", "nFeature_RNA", "barcode", "percent.mt", "RNA_snn_res.0.5", "TCR_pair_TCR1", "TCR_pair_cdr3_aa1", "TCR_pair_cdr3_nt1", "TCR_pair_TCR2", "TCR_pair_cdr3_aa2", "TCR_pair_cdr3_nt2", "TCR_pair_CTgene", "TCR_pair_CTnt", "TCR_pair_CTaa", "TCR_pair_CTstrict", "TCR_pair_cellType", "TCR_TRA_is_cell", "TCR_TRA_contig_id", "TCR_TRA_high_confidence", "TCR_TRA_length", "TCR_TRA_chain", "TCR_TRA_v_gene", "TCR_TRA_d_gene", "TCR_TRA_j_gene", "TCR_TRA_c_gene", "TCR_TRA_full_length", "TCR_TRA_productive", "TCR_TRA_fwr1", "TCR_TRA_fwr1_nt", "TCR_TRA_cdr1", "TCR_TRA_cdr1_nt", "TCR_TRA_fwr2", "TCR_TRA_fwr2_nt", "TCR_TRA_cdr2", "TCR_TRA_cdr2_nt", "TCR_TRA_fwr3", "TCR_TRA_fwr3_nt", "TCR_TRA_cdr3", "TCR_TRA_cdr3_nt", "TCR_TRA_fwr4", "TCR_TRA_fwr4_nt", "TCR_TRA_reads", "TCR_TRA_umis", "TCR_TRA_raw_clonotype_id", "TCR_TRA_raw_consensus_id", "TCR_TRA_exact_subclonotype_id", "TCR_TRB_is_cell", "TCR_TRB_contig_id", "TCR_TRB_high_confidence", "TCR_TRB_length", "TCR_TRB_chain", "TCR_TRB_v_gene", "TCR_TRB_d_gene", "TCR_TRB_j_gene", "TCR_TRB_c_gene", "TCR_TRB_full_length", "TCR_TRB_productive", "TCR_TRB_fwr1", "TCR_TRB_fwr1_nt", "TCR_TRB_cdr1", "TCR_TRB_cdr1_nt", "TCR_TRB_fwr2", "TCR_TRB_fwr2_nt", "TCR_TRB_cdr2", "TCR_TRB_cdr2_nt", "TCR_TRB_fwr3", "TCR_TRB_fwr3_nt", "TCR_TRB_cdr3", "TCR_TRB_cdr3_nt", "TCR_TRB_fwr4", "TCR_TRB_fwr4_nt", "TCR_TRB_reads", "TCR_TRB_umis", "TCR_TRB_raw_clonotype_id", "TCR_TRB_raw_consensus_id", "TCR_TRB_exact_subclonotype_id", "BCR_pair_IGH", "BCR_pair_cdr3_aa1", "BCR_pair_cdr3_nt1", "BCR_pair_IGLC", "BCR_pair_cdr3_aa2", "BCR_pair_cdr3_nt2", "BCR_pair_CTgene", "BCR_pair_CTnt", "BCR_pair_CTaa", "BCR_pair_CTstrict", "BCR_pair_cellType", "BCR_IGH_is_cell", "BCR_IGH_contig_id", "BCR_IGH_high_confidence", "BCR_IGH_length", "BCR_IGH_chain", "BCR_IGH_v_gene", "BCR_IGH_d_gene", "BCR_IGH_j_gene", "BCR_IGH_c_gene", "BCR_IGH_full_length", "BCR_IGH_productive", "BCR_IGH_fwr1", "BCR_IGH_fwr1_nt", "BCR_IGH_cdr1", "BCR_IGH_cdr1_nt", "BCR_IGH_fwr2", "BCR_IGH_fwr2_nt", "BCR_IGH_cdr2", "BCR_IGH_cdr2_nt", "BCR_IGH_fwr3", "BCR_IGH_fwr3_nt", "BCR_IGH_cdr3", "BCR_IGH_cdr3_nt", "BCR_IGH_fwr4", "BCR_IGH_fwr4_nt", "BCR_IGH_reads", "BCR_IGH_umis", "BCR_IGH_raw_clonotype_id", "BCR_IGH_raw_consensus_id", "BCR_IGH_exact_subclonotype_id", "BCR_IGL_is_cell", "BCR_IGL_contig_id", "BCR_IGL_high_confidence", "BCR_IGL_length", "BCR_IGL_chain", "BCR_IGL_v_gene", "BCR_IGL_d_gene", "BCR_IGL_j_gene", "BCR_IGL_c_gene", "BCR_IGL_full_length", "BCR_IGL_productive", "BCR_IGL_fwr1", "BCR_IGL_fwr1_nt", "BCR_IGL_cdr1", "BCR_IGL_cdr1_nt", "BCR_IGL_fwr2", "BCR_IGL_fwr2_nt", "BCR_IGL_cdr2", "BCR_IGL_cdr2_nt", "BCR_IGL_fwr3", "BCR_IGL_fwr3_nt", "BCR_IGL_cdr3", "BCR_IGL_cdr3_nt", "BCR_IGL_fwr4", "BCR_IGL_fwr4_nt", "BCR_IGL_reads", "BCR_IGL_umis", "BCR_IGL_raw_clonotype_id", "BCR_IGL_raw_consensus_id", "BCR_IGL_exact_subclonotype_id")
        #       meta_data_cols <- setdiff(meta_data_cols, exclude_cols)
        #       group_cols <- list()
        #       for(col in meta_data_cols) {
        #         group_cols[[col]] <- col
        #       }
        #       updateSelectInput(session, "group_by", choices = group_cols)
        #     }
        #   })
        # }

   # Update group_by
 update_group_by <- function(myReactives, session){   
    observe({
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        # 既存の選択肢を設定
        group_cols <- list("seurat_clusters" = "seurat_clusters", "sample" = "sample")
        # meta_dataの各列について、選択肢を追加
        meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
        for(col in meta_data_cols) {
          group_cols[[col]] <- col
        }
        updateSelectInput(session, "group_by", choices = group_cols)
      }
    })
 }

    # Update group_by
update_group_by2 <- function(myReactives){
  group_cols <- list("seurat_clusters" = "seurat_clusters", "sample" = "sample")
  meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
  for(col in meta_data_cols) {
    group_cols[[col]] <- col
  }

  if("TCR" %in% names(myReactives$seurat_object@meta.data)){
    group_cols[["TCR"]] <- "TCR"
  }

  if("BCR" %in% names(myReactives$seurat_object@meta.data)){
    group_cols[["BCR"]] <- "BCR"
  }
  return(group_cols)
}
        
# update_group_by2 <- function(myReactives){
#   observe({
#     if(!is.null(myReactives$seurat_object)) {
#       meta_data_cols <- names(myReactives$seurat_object@meta.data)
#       exclude_cols <- c("orig.ident", "nCount_RNA", "nFeature_RNA", "barcode", "percent.mt", "RNA_snn_res.0.5", "TCR_pair_TCR1", "TCR_pair_cdr3_aa1", "TCR_pair_cdr3_nt1", "TCR_pair_TCR2", "TCR_pair_cdr3_aa2", "TCR_pair_cdr3_nt2", "TCR_pair_CTgene", "TCR_pair_CTnt", "TCR_pair_CTaa", "TCR_pair_CTstrict", "TCR_pair_cellType", "TCR_TRA_is_cell", "TCR_TRA_contig_id", "TCR_TRA_high_confidence", "TCR_TRA_length", "TCR_TRA_chain", "TCR_TRA_v_gene", "TCR_TRA_d_gene", "TCR_TRA_j_gene", "TCR_TRA_c_gene", "TCR_TRA_full_length", "TCR_TRA_productive", "TCR_TRA_fwr1", "TCR_TRA_fwr1_nt", "TCR_TRA_cdr1", "TCR_TRA_cdr1_nt", "TCR_TRA_fwr2", "TCR_TRA_fwr2_nt", "TCR_TRA_cdr2", "TCR_TRA_cdr2_nt", "TCR_TRA_fwr3", "TCR_TRA_fwr3_nt", "TCR_TRA_cdr3", "TCR_TRA_cdr3_nt", "TCR_TRA_fwr4", "TCR_TRA_fwr4_nt", "TCR_TRA_reads", "TCR_TRA_umis", "TCR_TRA_raw_clonotype_id", "TCR_TRA_raw_consensus_id", "TCR_TRA_exact_subclonotype_id", "TCR_TRB_is_cell", "TCR_TRB_contig_id", "TCR_TRB_high_confidence", "TCR_TRB_length", "TCR_TRB_chain", "TCR_TRB_v_gene", "TCR_TRB_d_gene", "TCR_TRB_j_gene", "TCR_TRB_c_gene", "TCR_TRB_full_length", "TCR_TRB_productive", "TCR_TRB_fwr1", "TCR_TRB_fwr1_nt", "TCR_TRB_cdr1", "TCR_TRB_cdr1_nt", "TCR_TRB_fwr2", "TCR_TRB_fwr2_nt", "TCR_TRB_cdr2", "TCR_TRB_cdr2_nt", "TCR_TRB_fwr3", "TCR_TRB_fwr3_nt", "TCR_TRB_cdr3", "TCR_TRB_cdr3_nt", "TCR_TRB_fwr4", "TCR_TRB_fwr4_nt", "TCR_TRB_reads", "TCR_TRB_umis", "TCR_TRB_raw_clonotype_id", "TCR_TRB_raw_consensus_id", "TCR_TRB_exact_subclonotype_id", "BCR_pair_IGH", "BCR_pair_cdr3_aa1", "BCR_pair_cdr3_nt1", "BCR_pair_IGLC", "BCR_pair_cdr3_aa2", "BCR_pair_cdr3_nt2", "BCR_pair_CTgene", "BCR_pair_CTnt", "BCR_pair_CTaa", "BCR_pair_CTstrict", "BCR_pair_cellType", "BCR_IGH_is_cell", "BCR_IGH_contig_id", "BCR_IGH_high_confidence", "BCR_IGH_length", "BCR_IGH_chain", "BCR_IGH_v_gene", "BCR_IGH_d_gene", "BCR_IGH_j_gene", "BCR_IGH_c_gene", "BCR_IGH_full_length", "BCR_IGH_productive", "BCR_IGH_fwr1", "BCR_IGH_fwr1_nt", "BCR_IGH_cdr1", "BCR_IGH_cdr1_nt", "BCR_IGH_fwr2", "BCR_IGH_fwr2_nt", "BCR_IGH_cdr2", "BCR_IGH_cdr2_nt", "BCR_IGH_fwr3", "BCR_IGH_fwr3_nt", "BCR_IGH_cdr3", "BCR_IGH_cdr3_nt", "BCR_IGH_fwr4", "BCR_IGH_fwr4_nt", "BCR_IGH_reads", "BCR_IGH_umis", "BCR_IGH_raw_clonotype_id", "BCR_IGH_raw_consensus_id", "BCR_IGH_exact_subclonotype_id", "BCR_IGL_is_cell", "BCR_IGL_contig_id", "BCR_IGL_high_confidence", "BCR_IGL_length", "BCR_IGL_chain", "BCR_IGL_v_gene", "BCR_IGL_d_gene", "BCR_IGL_j_gene", "BCR_IGL_c_gene", "BCR_IGL_full_length", "BCR_IGL_productive", "BCR_IGL_fwr1", "BCR_IGL_fwr1_nt", "BCR_IGL_cdr1", "BCR_IGL_cdr1_nt", "BCR_IGL_fwr2", "BCR_IGL_fwr2_nt", "BCR_IGL_cdr2", "BCR_IGL_cdr2_nt", "BCR_IGL_fwr3", "BCR_IGL_fwr3_nt", "BCR_IGL_cdr3", "BCR_IGL_cdr3_nt", "BCR_IGL_fwr4", "BCR_IGL_fwr4_nt", "BCR_IGL_reads", "BCR_IGL_umis", "BCR_IGL_raw_clonotype_id", "BCR_IGL_raw_consensus_id", "BCR_IGL_exact_subclonotype_id")
#       meta_data_cols <- setdiff(meta_data_cols, exclude_cols)
#       for(col in meta_data_cols) {
#         group_cols[[col]] <- col
#       }
#       if("TCR" %in% names(myReactives$seurat_object@meta.data)){
#         group_cols[["TCR"]] <- "TCR"
#       }
    
#       if("BCR" %in% names(myReactives$seurat_object@meta.data)){
#         group_cols[["BCR"]] <- "BCR"
#       }
#       updateSelectInput(session, "group_by", choices = group_cols)
#     }
#   })
# }
