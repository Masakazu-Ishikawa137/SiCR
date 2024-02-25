findmarkerUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters", "sample")),
      selectInput(ns('ident1'), "ident.1", choices = c("Entire dataset" = "all")),
      uiOutput(ns('ident2_ui')),
      actionButton(ns("run"), "Run"),
      downloadButton(ns("download"), "Download table")
#      actionButton(ns('marker_run'), "Run_marker"),
#      numericInput(ns("num"), label = h3("Numeric input"), value = 5),
#      radioButtons(ns('choice'), 'Choice', choices = list('positive', 'negative', 'both'), selected = 'positive')
    ),
    mainPanel(
      DTOutput(ns('df_table'))
    )
  )
}

findmarkerServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {

    observeEvent(myReactives$seurat_object, {
      update_group_by(myReactives, session)
    })

    observe({
      if(!is.null(myReactives$seurat_object)){
        updateSelectInput(session, "ident1", choices = c('Entire dataset' = "all", sort(unique(myReactives$seurat_object[[]][[input$group_by]]))))
      }
    })

    output$ident2_ui <- renderUI({
      if (input$ident1 != 'all') {
            selectInput(session$ns('ident2'), "ident.2", choices = c('Entire dataset' = "all", sort(unique(myReactives$seurat_object[[]][[input$group_by]]))))
          }
        })

    myReactives$findmarker_df <- eventReactive(input$run, {
      req(input$run)
      if(input$ident1 == 'all') {
        FindAllMarkers(myReactives$seurat_object, group.by=input$group_by)
      } else if (input$ident1 != 'all' & input$ident2 == 'all') {
        FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, group.by=input$group_by)
      } else {
        FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, ident.2 = input$ident2, group.by=input$group_by)
      }
    })

    output$df_table <- renderDT({
      myReactives$findmarker_df()
    })

    output$download <- downloadHandler(
      filename = function() {
        paste("find_markers_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(myReactives$findmarker_df())  # データが存在することを確認
        write.csv(myReactives$findmarker_df(), file, row.names = FALSE)
      }
    )




#  # 修正されたobserveEvent(input$marker_run)ブロック
# observeEvent(input$marker_run, {
#   # Reactive expressionでデータ操作の結果を計算
#   data_for_table <- reactive({
#     if(input$choice == 'positive') {
#       sorted_data <- myReactives$findmarker_df %>%
#         arrange(desc(avg_log2FC))
#     } else if(input$choice == 'negative') {
#       sorted_data <- myReactives$findmarker_df %>%
#         arrange(avg_log2FC)
#     } else if(input$choice == 'both') {
#       sorted_data <- myReactives$findmarker_df %>%
#         arrange(desc(abs(avg_log2FC)))
#     }
#     sorted_data %>%
#       group_by(cluster) %>%
#       slice_head(n = input$num) %>%
#       summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
#   })

#   # Reactive expressionの結果をrenderTableに使用
#   output$marker_table <- renderTable({
#     data_for_table()
#   })
# })




  })
}