
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

antigenPredictionServer <- function(id, myReactives, chain = "TCR_TRB_raw_clonotype_id", db_path) {
  moduleServer(id, function(input, output, session) {

  if(chain == "TCR_TRB_raw_clonotype_id"){
    cdr3 <- "TCR_TRB_cdr3"
  } else if(chain == 'BCR_IGH_raw_clonotype_id'){
    cdr3 <- "BCR_IGH_cdr3"
  }

  joinData <- reactive({
    if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
      db <- read.delim(db_path)
      myReactives$seurat_object@meta.data %>%
        select(all_of(c(chain, cdr3))) %>%
          left_join(db, by=setNames("CDR3", cdr3))
      }
    })    
      
    observeEvent(joinData(), {
      # Get clonotype_ids of which cdr3 exist in the db, and set those to pulldown menu
      clonotype_ids <- joinData() %>%
        filter(if_any(starts_with("Epitope"), ~!is.na(.))) %>%
        pull(!!sym(chain)) %>%
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
        filter(!!sym(chain) == input$clonotype_id) %>%
        distinct()
    )
    
    # Download data
    output$download_data <- downloadHandler(
      filename = function() {paste0("antigen_prediction_", input$clonotype_id, ".csv")},
      content = function(file) {
        joinData() %>%
          filter(!!sym(chain) == input$clonotype_id) %>%
          distinct() %>%
          write_csv(file)
      }
    )
    
  })
}