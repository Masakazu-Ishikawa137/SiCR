heatmapUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("gene"), "Enter gene names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19"),
      downloadButton(ns('available_feature'), "You can download available gene name"),
      radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
#      checkboxInput(ns('rotate_plot'), "Rotate plot", value = FALSE),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF")

    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

heatmapServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
  plot_width <- reactive(input$plot_width)
  plot_height <- reactive(input$plot_height)
  legend <- reactive(input$legend)
  group_by <- reactive(input$group_by)
  rotate_plot <- reactive(input$rotate_plot)



  # Update group_by
  observeEvent(myReactives$seurat_object,{
    update_group_by(myReactives, session)
  })
  available_genes <- get_available_genes(reactive(myReactives$seurat_object))



observe({
  req(myReactives$seurat_object) # ここでmyReactives$seurat_objectがNULLでないことを確認
  if(!is.null(input$gene)){
    text_list <- unlist(strsplit(input$gene, ",\\s*"))

    # 利用可能な遺伝子のみをフィルタリング
    valid_genes <- text_list[text_list %in% available_genes()]
    
    if(length(valid_genes) > 0){
      # 有効な遺伝子が存在する場合、DotPlotを計算
      myReactives$heatmap <- DoHeatmap(myReactives$seurat_object, 
        features = valid_genes,
        group.by = group_by()) + theme(legend.position = legend())
    } else {
      # 有効な遺伝子がない場合、ユーザーに通知
      showNotification("指定された遺伝子がデータセットに存在しません。", type = "warning")
    }
  }
})

    render_plot(output, 'plot', reactive({ myReactives$heatmap }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$heatmap }))
    download_available_genes(output, input, available_genes())


  #   observeEvent(myReactives$seurat_object, {
  #     observe({
  #       if (!is.null(myReactives$seurat_object@misc$meta_data)) {
  #         meta_data_cols <- c("seurat_clusters", "sample", names(myReactives$seurat_object@misc$meta_data)) # nolint: line_length_linter.
  #         group_cols <- list() # group_colsを空のリストとして初期化
  #         for (col in meta_data_cols) {
  #           group_cols[[col]] <- col
  #         }
  #         updateSelectInput(session, "group_by", choices = group_cols)
  #       }
  #     })
  #   })

  #   observeEvent(input$gene,{
  #   if(!is.null(input$gene)){
  #     text_list <- unlist(strsplit(input$gene, ",\\s*"))
  #     output$plot <- renderPlot(DoHeatmap(myReactives$seurat_object, features = text_list) + NoLegend())
  #   }
  # })
})
}