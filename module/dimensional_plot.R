dimensional_plotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      selectInput(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
      uiOutput(ns('ncol_ui')),
      radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF") 

    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

dimensional_plotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)
    
    observeEvent(myReactives$seurat_object,{
      update_group_by(myReactives, session)
    })

  observeEvent(myReactives$seurat_object,{
    update_split_by(myReactives, session)
  })

  output$ncol_ui <- renderUI({
          if (input$split_by != 'none') {
            numericInput(session$ns('feature_column'), label = 'Number of columns', min = 1, value = 1)
          }
        })



    # Plot
    observe({
      if(!is.null(myReactives$seurat_object)){
        if(input$split_by == "none"){
          myReactives$dimplot <- DimPlot(
            myReactives$seurat_object,
            reduction = input$reduction,
            label = TRUE,
            pt.size = input$point_size,
            group.by = input$group_by,
            label.size = input$label_size
          ) + theme(legend.position = legend()) # nolint
        }
        else {
          myReactives$dimplot <- DimPlot(
            myReactives$seurat_object,
            reduction = input$reduction,
            label = TRUE,
            pt.size = input$point_size,
            group.by = input$group_by,
            split.by = input$split_by,
            label.size = input$label_size,
            ncol = input$feature_column
          ) + theme(legend.position = legend()) # nolint
        }
      }
    })
    render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
  })
}