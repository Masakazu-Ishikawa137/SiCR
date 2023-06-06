
uploadRdsUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Upload .rds file and put Load button"),
    ),
    mainPanel(
      fileInput(ns("rds"),  "Choose .rds  file"),
      textOutput(ns("text")),
      tableOutput(ns('cellnumber_table')),
    )
  )
}

uploadRdsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rds <- eventReactive(input$rds, {
        withProgress(message = "Now loading rds...", detail = "", value = 1/2, {
          readRDS(input$rds$datapath)
        })
      })
    
    observeEvent(rds(), {
      showNotification("Loading completed!")
      output$text <- renderText({ "The .rds file has been loaded." })
      output$cellnumber_table <- renderTable(
        rds()[["seurat_object"]]@meta.data %>%
          group_by(sample) %>%
          summarise("number of cell"=n())
      )
    })
    
    return(rds)

  }) # close moduleServer
}