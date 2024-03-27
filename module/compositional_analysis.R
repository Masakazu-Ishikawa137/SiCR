compositional_analysisUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
          radioButtons(ns('x_axis'), 'X axis', choices = c("sample", "seurat_clusters"), selected = 'sample'),
          radioButtons(ns('y_axis'), 'Group', choices = c("sample", "seurat_clusters"), selected = 'seurat_clusters'),
          radioButtons(ns('count'), 'Y axis', choices = c("count", "percentage")),
          radioButtons(ns('position'), 'Position', choices = c("stack", "dodge")),
          radioButtons(ns('flip'), "Flip axes", choices = c("vertical", "horizontal"), selected = "vertical"),
          legend_placeUI(ns),
          plot_sizeUI(ns),
#          scientific_colorUI(ns),
        ),
        mainPanel(
          DTOutput(ns('table')),
          downloadButton(ns('downloadTable'), 'Download table as CSV'),
          plotOutput(ns('plot')),
          downloadButton(ns("downloadPlot"), "Download Plot as PDF")

        )
      )
}

compositional_analysisServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)

    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "x_axis", choices = group_cols, selected = 'sample')
        updateRadioButtons(session, "y_axis", choices = group_cols, selected = 'seurat_clusters')
      })



    # df <- eventReactive(myReactives$seurat_object, {
    #   req(myReactives$seurat_object)
    #   return(myReactives$seurat_object@meta.data)
    # })

    df <- reactive({
      read.csv('metadata.csv')
    })


    df_table <- reactive({
      req(df())
      df_table <- as.data.frame(table(df()[[input$x_axis]], df()[[input$y_axis]]))
      names(df_table) <- c(input$x_axis, input$y_axis, "count")
      if (input$count == 'percentage') {
        df_table <- df_table %>%
          group_by(!!sym(input$x_axis)) %>%
          mutate(
            total_count = sum(count),
            percentage = (count / total_count) * 100
          ) %>%
          ungroup() %>%
          select(-total_count, -count)
      }
      return(df_table)
    })

    df_table_wide <- eventReactive(df_table(), {
      df_table() %>%
        pivot_wider(names_from = input$y_axis, values_from = input$count, values_fill = list(count = 0))
    })

    output$table <- renderDT({
      df_table_wide()
    })

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_table_wide(), file, row.names = FALSE)
      }
    )
    

    plot <- reactive({
      req(input$x_axis, input$y_axis)
      p <- df_table() %>%
        ggplot(aes_string(x = input$x_axis, y = input$count, fill = input$y_axis)) +
        geom_bar(stat = "identity", position = input$position) +
        theme_classic() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(legend.position = input$legend)
      if (input$flip == 'horizontal') {
        p <- p + coord_flip()
      }
      return(p)
    })

    render_plot(output, 'plot', reactive({ plot() }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ plot() }))
  })
}