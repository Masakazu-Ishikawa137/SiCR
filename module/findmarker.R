findmarkerUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters", "sample")),
      selectInput(ns('ident1'), "ident.1", choices = "all"),
      uiOutput(ns('ident2_ui')),
      actionButton(ns("run"), "Run")
    ),
    mainPanel(tableOutput(ns('df')))
  )
}

findmarkerServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    observeEvent(myReactives$seurat_object, {
      observe({
        if (!is.null(myReactives$seurat_object@misc$meta_data)) {
          meta_data_cols <- c("seurat_clusters", "sample", names(myReactives$seurat_object@misc$meta_data)) # nolint: line_length_linter.
          group_cols <- list() # group_colsを空のリストとして初期化
          for (col in meta_data_cols) {
            group_cols[[col]] <- col
          }
          updateSelectInput(session, "group_by", choices = group_cols)
        }
      })
      observe({
        updateSelectInput(session, "ident1", choices = c('all', unique(myReactives$seurat_object@meta.data[[input$group_by]]))) # nolint
        updateSelectInput(session, "ident2", choices = c('all', unique(myReactives$seurat_object@meta.data[[input$group_by]]))) # nolint: line_length_linter.
        output$ident2_ui <- renderUI({
          if (input$ident1 != 'all') {
            selectInput(session$ns('ident2'), "ident.2", choices = c('all', unique(myReactives$seurat_object@meta.data[[input$group_by]])))
          }
        })
      })
    })

    observeEvent(input$run,{
      if(input$ident1 == 'all'){
        output$df <- renderTable(FindAllMarkers(myReactives$seurat_object, group.by=input$group_by))
      }
      else if (input$ident1 != 'all' & input$ident2 == 'all'){
        output$df <- renderTable(FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, group.by=input$group_by))
      }
      else {
        output$df <- renderTable(FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, ident.2 = input$ident2, group.by=input$group_by))
      }
    })


  })
}