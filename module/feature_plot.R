featureplotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      reduction_methodUI(ns),
      gene_textUI(ns),
      downloadButton(ns('available_feature'), "You can download available gene name"),
      point_sizeUI(ns),
      feature_columnUI(ns),
      legend_placeUI(ns),
      plot_sizeUI(ns),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

featureplotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
  plot_width <- reactive(input$plot_width)
  plot_height <- reactive(input$plot_height)
  legend <- reactive(input$legend)
  ncol <- reactive(input$feature_column)

  observe({
    if(!is.null(myReactives$seurat_object)){
      myReactives$feature_plot <- featureplot(myReactives, input, output, session)
    }
  })

#   # Seuratオブジェクト内の遺伝子名を取得
#   available_genes <- get_available_genes(reactive(myReactives$seurat_object))
#   # available_genes <- eventReactive(myReactives$seurat_object,{
#   #   if(!is.null(myReactives$seurat_object)){
#   #     rownames(myReactives$seurat_object[["RNA"]])
#   #   }
#   # })

# observe({
# #observeEvent(input$gene,{
#   req(myReactives$seurat_object) # ここでmyReactives$seurat_objectがNULLでないことを確認
#   if(!is.null(input$gene)){
#     text_list <- unlist(strsplit(input$gene, ",\\s*"))

#     # 利用可能な遺伝子のみをフィルタリング
#     valid_genes <- text_list[text_list %in% available_genes()]
    
#     if(length(valid_genes) > 0){
#       # 有効な遺伝子が存在する場合、DotPlotを計算
#       myReactives$feature_plot <- FeaturePlot(myReactives$seurat_object, 
#       features = valid_genes, 
#       reduction = input$reduction,
#       pt.size = input$point_size,
#       ncol = ncol()) + theme(legend.position = legend())
#     } else {
#       # 有効な遺伝子がない場合、ユーザーに通知
#       showNotification("指定された遺伝子がデータセットに存在しません。", type = "warning")
#     }
#   }
# })

    render_plot(output, 'plot', reactive({ myReactives$feature_plot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$feature_plot }))
    download_available_genes(output, input, available_genes())




    # output$plot <- renderPlot({
    #   req(myReactives$feature_plot)
    #   myReactives$feature_plot
    # }, 
    # width = plot_width, height = plot_height)


    # output$downloadPlot <- downloadHandler(
    #   filename = function() {
    #     paste("feature_plot-", Sys.Date(), ".pdf", sep="")
    #   },
    #   content = function(file) {
    #     pdf(file, width = input$plot_width / 72, height = input$plot_height / 72)
    #     plot <- myReactives$feature_plot
    #     print(plot)
    #     dev.off()
    #   }
    # )


  })
}