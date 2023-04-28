geneUsageUI <- function(id, chain_choice) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("chain"), "Cain", choices = chain_choice),
      selectInput(ns("gene"), "Gene", choices = ""),
      selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
      radioButtons(ns("ver_or_hori"), "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
      checkboxInput(ns("legend"), "Show legend", value = TRUE),
      sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns("gene_usage_plot")),
      downloadButton(ns("download_data"), "Download data (.csv)"),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}



geneUsageServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    observe({
      if(input$chain == "TRA") {
        updateSelectInput(session, "gene", choices = list("v_gene",           "j_gene"))
      } else if (input$chain == "IGL") {
        updateSelectInput(session, "gene", choices = list("v_gene",           "j_gene", "c_gene"))
      } else if (input$chain == "TRB" | input$chain == "IGH") {
        updateSelectInput(session, "gene", choices = list("v_gene", "d_gene", "j_gene", "c_gene"))
      }
    })
    observe(updateSelectInput(session, "group_by", choices = group_cols))
    
    # get data
    geneUsageData <- reactive({
      data[[input$chain]]
    })
    
    # 集計
    geneUsageTally <- reactive({
      my_tally(
        geneUsageData(),
        drop_na = TRUE,
        group_by = input$group_by,
        arrange = FALSE,
        x = input$gene
      )
    })

    # Get plot
    geneUsagePlot <- reactive({
      if(input$ver_or_hori == "horizontal") {
        flip <- TRUE
      } else {
        flip <- FALSE
      }
      my_barplot(
        geneUsageTally(),
        x = input$gene,
        y = "count",
        fill = input$group_by,
        position = "dodge",
        legend_position = input$legend,
        flip = flip
      )
    })

    # output
    output$gene_usage_plot <- renderPlot(
      geneUsagePlot(),
      width  = plot_width,
      height = plot_height
    )
    
    output$download_data <- downloadHandler(
      filename = function() {"gene_usage.csv"},
      content = function(file) {
        write_csv(geneUsageTally(), file)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {"gene_usage.pdf"},
      content = function(file) {
        ggsave(file, plot = geneUsagePlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
      }
    )

  })
}