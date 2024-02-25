    # Update group_by
 update_split_by <- function(myReactives, session){   
    observe({
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        # 既存の選択肢を設定
        group_cols <- list("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample")
        # meta_dataの各列について、選択肢を追加
        meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
        for(col in meta_data_cols) {
          group_cols[[col]] <- col
        }
        updateSelectInput(session, "split_by", choices = group_cols)
      }
    })
 }
