dimentional_plotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns('reduction'), 'Dimentional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
      selectInput(ns("group_by"), "Group by", choices = "seurat_clusters"),
      selectInput(ns("palette"), "Color palette", choices = palette_list),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
      checkboxInput(ns("legend"), "Show legend", value = TRUE),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100)
    ),
    mainPanel(
      plotOutput(ns('clustering'))
    )
  )
}

dimentional_plotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(ifelse(input$legend, "right", "none"))
    
  observe({
    if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
      # 既存の選択肢を設定
      group_cols <- list("sample" = "sample", "seurat_clusters" = "seurat_clusters")
      # meta_dataの各列について、選択肢を追加
      meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
     for(col in meta_data_cols) {
        group_cols[[col]] <- col
      }
    updateSelectInput(session, "group_by", choices = group_cols)
    }
  })   # get alpha_diversity

     g <- reactive({
       if (!is.null(myReactives$seurat_object)) {
         my_colors <- my_palette(length(unique(myReactives$seurat_object@meta.data[[input$group_by]])), input$palette)
         DimPlot(
           myReactives$seurat_object,
           reduction = input$reduction,
           label = TRUE,
           pt.size = input$point_size,
           group.by = input$group_by,
           label.size = input$label_size
         ) +
         scale_color_manual(values = my_colors) +
         theme(legend.position = legend())
       }
     })

    output$clustering <- renderPlot({
      req(g())
      g()
    }, width = plot_width, height = plot_height)
  })
}