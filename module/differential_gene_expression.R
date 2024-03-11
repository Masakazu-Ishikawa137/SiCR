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
#      uiOutput(ns("thresholdui")),
      DTOutput(ns('df')),
      uiOutput(ns('download_button_ui')),
#      DTOutput(ns('marker_table'))
    )
  )
}

differential_gene_expressionServer <- function(id, myReactives) {
    moduleServer(id, function(input, output, session) {

        plot_width <- reactive(input$plot_width)
        plot_height <- reactive(input$plot_height)
        legend <- reactive(input$legend)

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "group_by", choices = group_cols)
      }
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

    observe({
      req(myReactives$findmarker_df)
      output$thresholdui <- renderUI({
        tagList(
          h3("Threshold"),
          radioButtons('p_value', "P value (Default: Adjusted P value)", choices = c("P value" = "p_val", "Adjusted P value" = 'p_val_adj'), selected = 'p_val_adj'),
          sliderInput('p_value_less', "is less than (Default: 0.1)", min = 0, max = 1, value = 0.1, step = 0.001),
          sliderInput('pct_less', "The proportion of cells in each cell group where the gene is expressed (Default: 0.01)", min = 0, max = 1, value = 0.01, step = 0.001),
          radioButtons(session$ns('choice'), 'Choice', choices = list('positive', 'negative', 'both'), selected = 'both'),
        )
      })
    })


    # observe({
    #   req(myReactives$findmarker_df, input$choice, input$p_value_less, input$p_value)
    #   myReactives$findmarker_df_final <- if(input$choice == 'positive') {
    #     myReactives$findmarker_df %>% dplyr::filter(avg_log2FC > 0)
    #   } else if(input$choice == 'negative') {
    #     myReactives$findmarker_df %>% dplyr::filter(avg_log2FC < 0)
    #   } else {
    #     myReactives$dindmarker_df_final <- myReactives$findmarker_df
    #   }

    #   myReactives$findmarker_df_final <- myReactives$findmarker_df_final %>%
    #     filter(.data[[input$p_value]] < input$p_value_less)
    # })


    output$df <- renderDT(myReactives$findmarker_df, filter = 'top')


    #   myReactives$findmarker_df <- myReactives$findmarker_df %>%
    #     filter(.data[[input$p_value]] < input$p_value_less[2] & .data[[input$p_value]] > input$p_value_less[1])
    # })



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

    })
}