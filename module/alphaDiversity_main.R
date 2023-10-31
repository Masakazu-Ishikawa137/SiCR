library("alakazam")

alphaDiversity_mainUI <- function(id) {
  ns <- NS(id)
  mainPanel(
    plotOutput(ns('alpha_diversity_plot')),
    downloadButton(ns("download_data"), "Download data (.csv)"),
    downloadButton(ns("download_plot"), "Download plot (.pdf)")
  )
}


alphaDiversity_mainServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(ifelse(input$legend, "right", "none"))
    observe(updateSelectInput(session, "group_by", choices = group_cols))
    
    # get alpha_diversity
    alphaDiversity <- reactive({
      data <- data %>%
        drop_na(any_of(c("raw_clonotype_id", input$group_by)))
      alakazam::alphaDiversity(
        data,
        clone  = "raw_clonotype_id",
        group  = input$group_by,
        min_q  = 0,
        max_q  = 4,
        step_q = 1,
        nboot  = 100,
      )
    })
    
    # get plot
    alphaDiversityPlot <- reactive({
      if (input$q == "all") {
        g <- plot(alphaDiversity())
      } else {
        g <- plot(alphaDiversity(), as.numeric(input$q))
      }
      if (!input$sd){
        g$layers <- g$layers[-1]
      }
      my_colors <- my_palette(length(unique(pull(alphaDiversity()@diversity, !!alphaDiversity()@group_by))), input$palette)
      g <- g +
        labs(title=NULL) +
        scale_color_manual(values = my_colors) +
        scale_fill_manual(values = my_colors) +
        theme_classic() +
        theme(
          legend.position = legend(),
          axis.text.x  = element_text(angle = 45, hjust = 1)
        )

      return(g)
    })

    # output plot
    output$alpha_diversity_plot <- renderPlot(
      alphaDiversityPlot(),
      width  = plot_width,
      height = plot_height
    )
    
    output$download_data <- downloadHandler(
      filename = function() {"alpha_diversity.csv"},
      content = function(file) {
        write_csv(alphaDiversity()@diversity, file)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {"alpha_diversity.pdf"},
      content = function(file) {
        ggsave(file, plot = alphaDiversityPlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
      }
    )
    
  })
}
