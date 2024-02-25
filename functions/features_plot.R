features_plot <- function(input, myReactives, available_genes, plotType) {
  observe({
    req(myReactives$seurat_object) # SeuratオブジェクトがNULLでないことを確認
    if(!is.null(input$gene)){
      text_list <- unlist(strsplit(input$gene, ",\\s*"))

      # 利用可能な遺伝子のみをフィルタリング
      valid_genes <- text_list[text_list %in% available_genes()]

      if(length(valid_genes) > 0){
        # 有効な遺伝子が存在する場合、指定されたプロットを計算
        plot <- switch(plotType,
                        "featurePlot" = FeaturePlot(myReactives$seurat_object, 
                                                    features = valid_genes, 
                                                    reduction = input$reduction,
                                                    pt.size = input$point_size,
                                                    ncol = ncol()) + theme(legend.position = legend()),
                        "heatmap" = DoHeatmap(myReactives$seurat_object, 
                                              features = valid_genes,
                                              group.by = input$group_by) + theme(legend.position = legend())
        )
        if(plotType == "featurePlot") {
          myReactives$feature_plot <- plot
        } else if(plotType == "heatmap") {
          myReactives$heatmap <- plot
        }
      } else {
        # 有効な遺伝子がない場合、ユーザーに通知
        showNotification("指定された遺伝子がデータセットに存在しません。", type = "warning")
      }
    }
  })
}