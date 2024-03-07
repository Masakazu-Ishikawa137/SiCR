dimensional_plotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      reduction_methodUI(ns),
      group_byUI(ns),
      split_byUI(ns),
      point_sizeUI(ns),
      label_sizeUI(ns),
      uiOutput(ns('ncol_ui')),
      legend_placeUI(ns),
      plot_sizeUI(ns),
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
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by2(myReactives)
        updateRadioButtons(session, "group_by", choices = group_cols)
      }
    })

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols_split <- update_split_by_new(myReactives)
        group_cols_split[["None"]] <- "none"
        updateRadioButtons(session, "split_by", choices = group_cols_split, selected = "none")
      }
    })

  output$ncol_ui <- renderUI({
          if (input$split_by != 'none') {
            numericInput(session$ns('feature_column'), label = 'Number of columns', min = 1, value = 1)
          }
        })

    observe({
      if(!is.null(myReactives$seurat_object)){
        myReactives$dimplot <- dimplot(myReactives, input, output, session)
      }
    })

    render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
  })
}