dimensional_plotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      reduction_methodUI(ns),
      group_byUI(ns),
      split_byUI(ns),
      point_sizeUI(ns),
      label_sizeUI(ns),
      uiOutput(ns("ncol_ui")),
      legend_placeUI(ns),
      plot_sizeUI(ns),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    ),
    mainPanel(
      plotOutput(ns("plot"))
    )
  )
}

dimensional_plotServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)


    # update group by
    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- group_col(myReactives)
        if ("TCR" %in% names(myReactives$seurat_object@meta.data)) {
          group_cols[["TCR"]] <- "TCR"
          group_cols[["TCR_count_per_sample"]] <- "TCR_count_per_sample"
          group_cols[["TCR_expand"]] <- "TCR_expand"
        }

        if ("BCR" %in% names(myReactives$seurat_object@meta.data)) {
          group_cols[["BCR"]] <- "BCR"
          group_cols[["BCR_count_per_sample"]] <- "BCR_count_per_sample"
          group_cols[["BCR_expand"]] <- "BCR_expand"
        }

        updateRadioButtons(session, "group_by", choices = group_cols)
      }
    })


    # update split by
    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- group_col(myReactives)
        group_cols[["None"]] <- "none"
        updateRadioButtons(session, "split_by", choices = group_cols, selected = "none")
      }
    })

    output$ncol_ui <- renderUI({
      if (input$split_by != "none") {
        numericInput(session$ns("feature_column"), label = "Number of columns", min = 1, value = 1)
      }
    })

    observe({
      if (!is.null(myReactives$seurat_object)) {
        myReactives$dimplot <- dimplot(myReactives, input, output, session)
      }
    })

    render_plot(output, "plot", reactive({
      myReactives$dimplot
    }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({
      myReactives$dimplot
    }))
  })
}
