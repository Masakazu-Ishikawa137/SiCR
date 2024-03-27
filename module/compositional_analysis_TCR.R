compositional_analysis_TCRUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
          radioButtons(ns('x_axis'), 'X axis', choices = c("sample", "seurat_clusters"), selected = 'sample'),
          radioButtons(ns('flip'), "Flip axes", choices = c("vertical", "horizontal"), selected = "vertical"),
          plot_sizeUI(ns),
        ),
        mainPanel(
          DTOutput(ns('table')),
          downloadButton(ns('downloadTable'), 'Download table as CSV'),
          plotOutput(ns('plot')),
          downloadButton(ns("downloadPlot"), "Download Plot as PDF")

        )
      )
}

compositional_analysis_TCRServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)

    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "x_axis", choices = group_cols, selected = 'sample')
      })



    df <- eventReactive(myReactives$seurat_object, {
      req(myReactives$seurat_object)
      return(myReactives$seurat_object@meta.data)
    })

    # df <- reactive({
    #   read.csv('metadata.csv')
    # })

    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "x_axis", choices = group_cols, selected = 'sample')
      })

    df_table <- reactive({
      req(df())

      csv <- csv %>% 
        dplyr::distinct(input$x_axis, TCR_pair_CTaa, .keep_all = TRUE)
      
      csv <- as.data.frame(table(csv[[input$x_axis]]))
      print(csv)
      names(csv) <- c(input$x_axis, "Number")
       print(csv)
    })

    output$table <- renderDT({
      df_table()
    })
      
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_table(), file, row.names = FALSE)
      }
    )
    

    plot <- reactive({
      req(df_table())
      ggplot(df_table(), aes_string(x = input$x_axis, y = 'Number')) +
        geom_bar(stat = "identity") +
        theme_classic() +
        scale_y_continuous(expand = c(0, 0))
    })

    render_plot(output, 'plot', reactive({ plot() }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ plot() }))
  })
}