modify_tbcr <- function(tbcr_df, seurat_object_metadata){
  # tcr/bcr basically has 2 lines per barcode (dimer)
  # filter by chain should be prior to distinct!
  tbcr_modified <- tbcr_df %>%
    group_by(chain) %>%
    nest()
  # convert to list by 'chain' value
  tbcr_lst <- tbcr_modified$data %>%
    set_names(tbcr_modified$chain)
  tbcr_lst <- map(tbcr_lst,
                  ~ .x %>%
                    distinct(barcode, .keep_all = TRUE) %>%
                    # add celltype column from seurat_object@meta.data
                    left_join(select(seurat_object_metadata, barcode, celltype), by = "barcode")
  )
  return(tbcr_lst)
}