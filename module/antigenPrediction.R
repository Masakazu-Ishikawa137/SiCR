
antigenPredictionUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("clonotype_id"), "Clonotype id", choices = "")
    ),
    mainPanel(
      tableOutput(ns("table")),
      downloadButton(ns("download_data"), "Download data (.csv)")
    )
  )
}

antigenPredictionServer <- function(id, data, db_path) {
  moduleServer(id, function(input, output, session) {
    
      joinData <- reactive({
        if (!is.null(data)){ # To prevent an error when there is no input for TCR or BCR.
          db <- read.delim(db_path)
          data %>%
            select(all_of(c("raw_clonotype_id", "cdr3"))) %>%
            left_join(db, by=c("cdr3" = "CDR3"))
        }
      })
    
      
    observeEvent(joinData(), {
      # Get clonotype_ids of which cdr3 exist in the db, and set those to pulldown menu
      clonotype_ids <- joinData() %>%
        filter(if_any(starts_with("Epitope"), ~!is.na(.))) %>%
        pull(raw_clonotype_id) %>%
        unique()
      clonotype_ids_order <- clonotype_ids %>%
        str_remove("clonotype") %>%
        as.numeric() %>%
        order
      clonotype_ids <- clonotype_ids[clonotype_ids_order]
      updateSelectInput(session, "clonotype_id", choices = clonotype_ids)
    })
    
    # Output table
    output$table <- renderTable(
      joinData() %>%
        filter(raw_clonotype_id == input$clonotype_id) %>%
        distinct()
    )
    
    # Download data
    output$download_data <- downloadHandler(
      filename = function() {paste0("antigen_prediction_", input$clonotype_id, ".csv")},
      content = function(file) {
        joinData() %>%
          filter(raw_clonotype_id == input$clonotype_id) %>%
          distinct() %>%
          write_csv(file)
      }
    )
    
  })
}