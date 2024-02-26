findmarkerUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters", "sample")),
      selectInput(ns('ident1'), "ident.1", choices = "all"),
      uiOutput(ns('ident2_ui')),
      actionButton(ns("run"), "Run"),
      downloadButton(ns("download"), "Download CSV"),
      uiOutput(ns('marker')) # ここでUIの出力を定義
    ),
    mainPanel(
      DTOutput(ns('df')),
      DTOutput(ns('marker_table'))
    )
  )
}

findmarkerServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    observeEvent(myReactives$seurat_object,{
      update_group_by(myReactives, session)
    })
    
    observe({
      if(!is.null(myReactives$seurat_object)){
        updateSelectInput(session, "ident1", choices = c('all', sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
        output$ident2_ui <- renderUI({
          if (input$ident1 != 'all') {
            selectInput(session$ns('ident2'), "against", choices = c('all', sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
          }
        })
      }
    })

    observeEvent(input$run,{
      if(input$ident1 == 'all'){
        myReactives$findmarker_df <- FindAllMarkers(myReactives$seurat_object, group.by=input$group_by)
      }
      else if (input$ident1 != 'all' & input$ident2 == 'all'){
        myReactives$findmarker_df <- FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, group.by=input$group_by)
      }
      else {
        myReactives$findmarker_df <- FindMarkers(myReactives$seurat_object, ident.1 = input$ident1, ident.2 = input$ident2, group.by=input$group_by)
      }
      output$df <- renderDT(myReactives$findmarker_df)
    })

    output$download <- downloadHandler(
      filename = function() {
        paste("findmarkers-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        data <- myReactives$findmarker_df
        write.csv(data, file, row.names = FALSE)
      }
    )

    observeEvent(myReactives$findmarker_df,{
      output$marker <- renderUI({
        if(input$ident1 == 'all'){ # この条件を変更または削除して、必要に応じて表示を調整
          tagList(
            numericInput(session$ns("num"), label = "Number of genes", value = 5),
            radioButtons(session$ns('choice'), 'Choice', choices = list('positive', 'negative', 'both'), selected = 'positive'),
            downloadButton(session$ns("download_marker"), "Download marker table"),

          )
        }
      })
    })

      data_for_table <- reactive({
      req(myReactives$findmarker_df, input$choice) # 必要なデータが存在することを保証

      sorted_data <- if(input$choice == 'positive') {
        myReactives$findmarker_df %>% arrange(desc(avg_log2FC))
      } else if(input$choice == 'negative') {
        myReactives$findmarker_df %>% arrange(avg_log2FC)
      } else {
        myReactives$findmarker_df %>% arrange(desc(abs(avg_log2FC)))
      }

      if (any(grepl("cluster", colnames(sorted_data)))) {
        sorted_data %>%
          group_by(cluster) %>%
          slice_head(n = input$num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      } else {
        sorted_data %>%
          slice_head(n = input$num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      }
    })

    # data_for_tableを使用してテーブルをレンダリング
    output$marker_table <- renderDT({
      data_for_table() # reactive expressionを呼び出し
    })

    output$download_marker <- downloadHandler(
      filename = function() {
        paste("markers-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        data <- data_for_table() 
        write.csv(data, file, row.names = FALSE)
      }
    )


    # observe({
    #   if(!(is.null(myReactives$findmarker_df))){
    #     # Reactive expressionでデータ操作の結果を計算
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
    #     if (any(grepl("cluster", colnames(sorted_data)))){
    #       data_for_table <- sorted_data %>%
    #         group_by(cluster) %>%
    #           slice_head(n = input$num) %>%
    #             summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
    #     } else {
    #       data_for_table <- sorted_data %>%
    #         slice_head(n = input$num) %>%
    #           summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
    #     }
    #   output$marker_table <- renderTable({data_for_table}) 
    #   }
    # })

  })
}


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
