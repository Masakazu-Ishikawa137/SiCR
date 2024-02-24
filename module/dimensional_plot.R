dimensional_plotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
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

    # Plot
    observe({
      if(!is.null(myReactives$seurat_object)){
        myReactives$dimplot <- DimPlot(
          myReactives$seurat_object,
          reduction = input$reduction,
          label = TRUE,
          pt.size = input$point_size,
          group.by = input$group_by,
          label.size = input$label_size
        ) + theme(legend.position = legend()) # nolint
      }
    })
    render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
  })
}