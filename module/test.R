library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Rank List Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter items (comma-separated):"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("output_text")
    )
  )
)

# Define server
server <- function(input, output) {
  # Store the items entered by the user
  items <- reactiveValues(list = NULL)
  
  # Update the list of items when the submit button is clicked
  observeEvent(input$submit_button, {
    items$list <- strsplit(input$input_text, ",")[[1]]
  })
  
  # Render the rank list
  output$output_text <- renderPrint({
    if (!is.null(items$list)) {
      rank_list(items$list)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)