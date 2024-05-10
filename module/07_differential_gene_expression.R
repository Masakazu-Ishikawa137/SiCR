differential_gene_expressionUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        group_byUI(ns),
        ident1UI(ns),
        ident2UI(ns),
        runbuttonUI(ns),
        uiOutput(ns('marker')) # ここでUIの出力を定義
    ),
    mainPanel(
      DTOutput(ns('df')),
#       uiOutput(ns('dynamic_gene_num_input')),
#      numericInput(ns('gene_num'), label = "Number of genes", value = 10),
#      uiOutput(ns('gene_num')),
#      DTOutput(ns('marker_table_positive')),
#      DTOutput(ns('marker_table_negative')),
#      DTOutput(ns('marker_table')),
#      DTOutput(ns('negative_table'))
    )
  )
}

differential_gene_expressionServer <- function(id, myReactives) {
    moduleServer(id, function(input, output, session) {

        plot_width <- reactive(input$plot_width)
        plot_height <- reactive(input$plot_height)
        legend <- reactive(input$legend)

    observeEvent(myReactives$seurat_object,{
      update_group_by(session, myReactives)
    })

    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object)) {
        updateSelectInput(session, "ident1", choices = c("All group" = "all", sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
      }
    })

    observeEvent(input$ident1, {
      if (input$ident1 != "all") {
        updateSelectInput(session, "ident2", choices = c("Other groups" = "all", sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
      } else {
        updateSelectInput(session, "ident2", choices = c("Other groups" = "all"))
      }
    })

    observeEvent(input$run,{
        Idents(myReactives$seurat_object) <- input$group_by
        if(input$ident1 == 'all'){
            myReactives$findmarker_df <- FindAllMarkers(myReactives$seurat_object,
            logfc.threshold =0,
            min.pct = 0.01)
        }
        else if (input$ident1 != 'all' & input$ident2 == 'all'){
            myReactives$findmarker_df <- FindMarkers(myReactives$seurat_object, 
            ident.1 = input$ident1,
            logfc.threshold = 0,
            min.pct = 0.01)
        }
        else {
            myReactives$findmarker_df <- FindMarkers(myReactives$seurat_object, 
            ident.1 = input$ident1, 
            ident.2 = input$ident2,
            logfc.threshold = 0,
            min.pct = 0.01)
        }
       })


    output$df <- renderDT(myReactives$findmarker_df, filter = 'top')

    output$download_button_ui <- renderUI({
      if(!is.null(myReactives$findmarker_df) && nrow(myReactives$findmarker_df) > 0) {
        downloadButton(session$ns('download'), 'Download table as CSV')
      }
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
    
    output$dynamic_gene_num_input <- renderUI({
      if (!is.null(myReactives$findmarker_df) && nrow(myReactives$findmarker_df) > 0) {
        tagList(
          h3("Most differentially expressed genes (p_val_adj < 0.1):"),
          numericInput(session$ns('gene_num'), label = "Number of genes", value = 10)
        )
      }
    })

    data_for_table_positive <- reactive({
      req(myReactives$findmarker_df) # 必要なデータが存在することを保証
      sorted_data <- myReactives$findmarker_df %>% 
        dplyr::filter(p_val_adj < 0.1) %>%
          arrange(desc(avg_log2FC))


      if (any(grepl("cluster", colnames(sorted_data)))) {
        sorted_data %>%
          group_by(cluster) %>%
          slice_head(n = input$gene_num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      } else {
        sorted_data %>%
          slice_head(n = input$gene_num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      }
    })

    data_for_table_negative <- reactive({
      req(myReactives$findmarker_df) # 必要なデータが存在することを保証
      sorted_data <- myReactives$findmarker_df %>% 
        dplyr::filter(p_val_adj < 0.1) %>%
          arrange(avg_log2FC)


      if (any(grepl("cluster", colnames(sorted_data)))) {
        sorted_data %>%
          group_by(cluster) %>%
          slice_head(n = input$gene_num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      } else {
        sorted_data %>%
          slice_head(n = input$gene_num) %>%
          summarise(genes = paste(gene, collapse = ', '), .groups = 'drop')
      }
    })

    output$marker_table_positive <- renderDT({
      data_for_table_positive() # reactive expressionを呼び出し
    })

    output$marker_table_negative <- renderDT({
      data_for_table_negative() # reactive expressionを呼び出し
    })




    # data_for_tableを使用してテーブルをレンダリング
    output$marker_table <- renderDT({
      data_for_table() # reactive expressionを呼び出し
    })

    })
}