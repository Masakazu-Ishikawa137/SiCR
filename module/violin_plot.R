ViolinPlotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("gene"), "Enter gene names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19"),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      selectInput(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      numericInput(ns('feature_column'), label = 'Number of columns', min = 1, value = 1),
      sliderInput(ns("point_size"), "Size of points", min = 0, max = 1, value = 0.1, step = 0.01),
      checkboxInput(ns("stack"), "Stack", value = FALSE),
      checkboxInput(ns("flip"), "Flip", value = FALSE),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    ),
    mainPanel(plotOutput(ns('plot')))
  )
}

ViolinPlotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){

  plot_width <- reactive(input$plot_width)
  plot_height <- reactive(input$plot_height)
  legend <- reactive(input$legend)

#  ncol <- reactive(input$feature_column)
#  group_by <- reactive(input$group_by)


  observeEvent(myReactives$seurat_object,{
    update_group_by(myReactives, session)
  })

  observeEvent(myReactives$seurat_object,{
    update_split_by(myReactives, session)
  })

  # split_by <- reactive({
  #   if (input$split_by == "none") {
  #     # "None"が選択された場合の処理
  #     NULL
  #   } else {
  #     # その他の値が選択された場合の処理
  #     input$split_by
  #   }
  # })

  # Seuratオブジェクト内の遺伝子名を取得
  available_genes <- get_available_genes(reactive(myReactives$seurat_object))

observe({
  req(myReactives$seurat_object) # ここでmyReactives$seurat_objectがNULLでないことを確認
  if(!is.null(input$gene)){
    text_list <- unlist(strsplit(input$gene, ",\\s*"))

    # 利用可能な遺伝子のみをフィルタリング
    valid_genes <- text_list[text_list %in% available_genes()]
    
    if(length(valid_genes) > 0){
      # 有効な遺伝子が存在する場合、DotPlotを計算
      if(input$split_by == "none"){
        myReactives$violin_plot <- VlnPlot(myReactives$seurat_object, 
        features = valid_genes,
        group.by = input$group_by,
        split.by = NULL,
        ncol = input$feature_column,
        pt.size = input$point_size,
        stack = input$stack,
        flip = input$flip)
      } else{
        myReactives$violin_plot <- VlnPlot(myReactives$seurat_object, 
        features = valid_genes,
        group.by = input$group_by,
        split.by = input$split_by,
        ncol = input$feature_column,
        pt.size = input$point_size,
        stack = input$stack,
        flip = input$flip)
      }
#      group.by = group_by(),
#      ncol = ncol())
    } else {
      # 有効な遺伝子がない場合、ユーザーに通知
      showNotification("指定された遺伝子がデータセットに存在しません。", type = "warning")
    }
  }
})

    render_plot(output, 'plot', reactive({ myReactives$violin_plot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$violin_plot }))




})
}