library("alakazam")

alphaDiversityUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
      radioButtons(ns("legend"), "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns('alpha_diversity_plot'))
    )
  )
}


alphaDiversityServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    observe(updateSelectInput(session, "group_by", choices = group_cols))
    
    # get alpha_diversity
    alphaDiversityPlot <- reactive({
      data <- data %>%
        drop_na(any_of(c("raw_clonotype_id", input$group_by)))
      alpha_diversity <- alakazam::alphaDiversity(
        data,
        clone  = "raw_clonotype_id",
        group  = input$group_by,
        min_q  = 0,
        max_q  = 4,
        step_q = 1,
        nboot  = 100,
      )
      plot(alpha_diversity)
    })

    # output plot
    output$alpha_diversity_plot <- renderPlot(
      alphaDiversityPlot() +
        theme_classic() +
        scale_fill_nejm() +
        ggtitle(NULL) +
        scale_color_nejm() +
        theme(legend.position=input$legend),
      width  = plot_width,
      height = plot_height
    )
  })
}
