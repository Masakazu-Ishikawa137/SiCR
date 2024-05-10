cdr3_spectratypingUI <- function(id, chain = "TCR") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      chain_selectionUI(ns, chain),
      sequence_selectionUI(ns),
      group_byUI(ns),
      focus_groupUI(ns),
      plot_size_and_download_pdfUI(ns),
      downloadButton(ns('downloadTable'), 'Download table as CSV'),

    ),
    mainPanel(
      DTOutput(ns("table")),
      plotOutput(ns("plot")),
    )
  )
}

cdr3_spectratypingServer <- function(id, myReactives, chain = "TCR") {
  moduleServer(id, function(input, output, session) {
    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
      update_group_by(session, myReactives)
    })


    toListen <- reactive({
      list(myReactives$seurat_object, input$group_by)
    })

    observeEvent(toListen(), {
      req(myReactives$seurat_object)
      myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
      updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = myReactives$new_choices)
    })

    input_lists <- reactive({
      list(input$chain, input$sequence, input$group_by, input$focus_group)
    })


    observeEvent(input_lists(), {
      req(myReactives$seurat_object)
      if (input$sequence == "aa") {
        gene_column <- paste0(chain, "_", input$chain, "_", "cdr3")
      } else if (input$sequence == "nt") {
        gene_column <- paste0(chain, "_", input$chain, "_", "cdr3_nt")
      }

      df <- myReactives$seurat_object@meta.data %>% dplyr::filter(.data[[input$group_by]] %in% input$focus_group)
      df$length <- str_length(df[[gene_column]])
      df <- df %>% select(input$group_by, gene_column, length)
      df <- na.omit(df)
      myReactives$cdr3_spectratyping_df <- df

      # barplotを作成
      myReactives$cdr3_spectratyping_plot <- ggplot(myReactives$cdr3_spectratyping_df, aes(x = length, fill = !!sym(input$group_by))) +
        geom_bar(stat = "count", position = "dodge") +
        theme_classic() +
        scale_y_continuous(expand = c(0, 0))
    })

    render_plot(
      output,
      "plot",
      reactive({
        myReactives$cdr3_spectratyping_plot
      }),
      reactive({
        input$plot_width
      }),
      reactive({
        input$plot_height
      })
    )

    output$table <- renderDT({
      myReactives$cdr3_spectratyping_df
    })

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(myReactives$cdr3_spectratyping_df, file, row.names = FALSE)
      }
    )

    setupDownloadPlotHandler(output, input, reactive({
      myReactives$highlight_plot
    }))
  })
}


