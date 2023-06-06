library("alakazam")

alphaDiversityUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
      selectInput(ns("palette"), "Color palette", choices = palette_list),
      selectInput(ns("q"), "Order of the Hill number (q)", choices = list("all (1-4)" = "all", "1 (Shannon diversity)" = 1, "2 (Simpson’s index)" = 2)),
      checkboxInput(ns("sd"), "Show SD", value = TRUE),
      checkboxInput(ns("legend"), "Show legend", value = TRUE),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns('alpha_diversity_plot')),
      downloadButton(ns("download_data"), "Download data (.csv)"),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}


alphaDiversityServer <- function(id, data, group_cols) {
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
