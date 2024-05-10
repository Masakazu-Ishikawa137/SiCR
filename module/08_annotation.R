annotation_newUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      reduction_methodUI(ns),
      group_byUI(ns),
      textInput(ns('new_group'), label = "Or add new group"),
      actionButton(ns('add_group'), label = "Add Group"),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 3, step = 0.01),
      textInput(ns("cell_annotation"), label = "Label"),
      actionButton(ns("button"), label = "Press"),

    ),
    mainPanel(
      plotlyOutput(ns("plot")),
      verbatimTextOutput(ns("selectedData")),

    )
  )
}

annotation_newServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)


    # update group.by
    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
      update_group_by(session, myReactives)
    })

    df <- reactive({
      req(myReactives$seurat_object)
      if (input$reduction == "umap") {
        coodinate <- myReactives$seurat_object[["umap"]]@cell.embeddings
      } else if (input$reduction == "tsne") {
        coodinate <- myReactives$seurat_object[["tsne"]]@cell.embeddings
      }
      coodinate_named <- as.data.frame(coodinate)
      names(coodinate_named) <- c("x", "y")
      df <- cbind(myReactives$seurat_object@meta.data, coodinate_named)
    })

    #  df <- reactive({
    #    df <- read.csv("/user/ifrec/mishikawa/SiCR/df.csv")
    #    mutate(df, new_column = NA)
    #  })

    output$plot <- renderPlotly({
      plot_ly(
        data = df(),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "markers",
        color = ~ .data[[input$group_by]],
        marker = list(size = input$point_size)
        #    marker = list(size = input$point_size, color = ~.data[[input$group_by]]),
      ) %>%
        layout(width = input$plot_width, height = input$plot_height)
    })

    output$selectedData <- renderPrint({
      # 選択されたデータポイントの情報を取得
      s <- event_data("plotly_selected")
      if (is.null(s)) {
        "細胞を選択してください。"
      } else {
        myReactives$selected_barcodes <- left_join(s, df(), by = c("x", "y")) %>% .$barcode
        return(paste(length(myReactives$selected_barcodes), 'cells are selected'))
        # df <- df() %>% mutate(new_column = case_when(
        #   barcode %in% barcodes ~ "your_value",
        #   TRUE ~ NA
        # ))
        # df
        # df()のresult$barcodeと同じ行のnew_columnに値を設定
        #    df() <- df() %>% dplyr::filter(barcode %in% result$barcode)
        #    result$barcode
        #      print(s)
        #      selected_cells <- sample_data[s$pointNumber + 1, ]
        #      paste("選択された細胞のタイプ:", toString(unique(selected_cells$CellType)))
      }
    })

    observeEvent(input$add_group, {
      new_group <- input$new_group
      myReactives$seurat_object@meta.data$new_group <- 'none'
    })


    observeEvent(input$button, {
      selected_group <- input$group_by
      selected_barcode <- myReactives$selected_barcodes
      labeled <- input$cell_annotation
      df <- df() %>% mutate(!!input$group_by := case_when(
        barcode %in% selected_barcode ~ labeled,
        TRUE ~ as.character(df()[[input$group_by]])
      ))
      df <- df %>% select(-x, -y)
      myReactives$seurat_object@meta.data <- df
      print(head(df))
      
    })
  })
}
# mtcars %>%
#                tibble::rownames_to_column(var = "cars_name") %>% 
#                tibble::as_tibble() %>% 
#                mutate( !!sym(input$new_name) := paste0("new_", cars_name))

# plotlyUI <- function(id){
#   ns <- NS(id)
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
#       selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
#       selectInput(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample")),
#       sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
#       sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 5, step = 1),
#       uiOutput(ns('ncol_ui')),
#       radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
#       sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
#       sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
#       downloadButton(ns("downloadPlot"), "Download Plot as PDF")

#     ),
#     mainPanel(
#       plotlyOutput(ns('plot')),
#        verbatimTextOutput("selectedData")

#     )
#   )
# }

# plotlyServer <- function(id, myReactives){
#   moduleServer(id, function(input, output, session){
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)
#     legend <- reactive(input$legend)

#     observeEvent(myReactives$seurat_object,{
#       if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         group_cols <- update_group_by2(myReactives)
#         updateSelectInput(session, "group_by", choices = group_cols)
#       }
#     })


#   observeEvent(myReactives$seurat_object,{
#     update_split_by(myReactives, session)
#   })

#   output$ncol_ui <- renderUI({
#           if (input$split_by != 'none') {
#             numericInput(session$ns('feature_column'), label = 'Number of columns', min = 1, value = 1)
#           }
#         })



#     # Plot
#     observe({
#       if(!is.null(myReactives$seurat_object)){
#         if(input$split_by == "none"){
#           myReactives$dimplot <- DimPlot(
#             myReactives$seurat_object,
#             reduction = input$reduction,
#             label = TRUE,
#             pt.size = input$point_size,
#             group.by = input$group_by,
#             label.size = input$label_size
#           ) + theme(legend.position = legend()) # nolint
#         }
#         else {
#           myReactives$dimplot <- DimPlot(
#             myReactives$seurat_object,
#             reduction = input$reduction,
#             label = TRUE,
#             pt.size = input$point_size,
#             group.by = input$group_by,
#             split.by = input$split_by,
#             label.size = input$label_size,
#             ncol = input$feature_column
#           ) + theme(legend.position = legend()) # nolint
#         }
#       }
#     })

#   output$plot <- renderPlotly({
#   req(myReactives$dimplot)
#   plot <- ggplotly(myReactives$dimplot) %>% layout(width = plot_width(), height = plot_height(),
#     legend = list(
#       font = list(size = 10), # フォントサイズの調整
#         tracegroupgap = 1 # グループ間のスペースの調整
#     )
#   )
#   plot
# })
#   output$selectedData <- renderPrint({
#     # 選択されたデータポイントの情報を取得
#     s <- event_data("plotly_selected")
#     if (is.null(s)) {
#       "細胞を選択してください。"
#     } else {
#       selected_cells <- sample_data[s$pointNumber + 1, ]
#       paste("選択された細胞のタイプ:", toString(unique(selected_cells$CellType)))
#     }
#   })

#     setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
#   })
# }


# plotlyUI <- function(id) {
#   ns <- NS(id)
#   sidebarLayout(
#     sidebarPanel(
#       reduction_methodUI(ns),
#       group_byUI(ns),
#       point_sizeUI(ns),
#       label_sizeUI(ns),
#       legend_placeUI(ns),
#       plot_sizeUI(ns),
#       downloadButton(ns("download_marker"), "Download CSV"),
#       downloadButton(ns("downloadPlot"), "Download Plot as PDF"),
#       actionButton(ns("button"), label = "Press"),
#     ),
#     mainPanel(
#       plotlyOutput(ns("plot")),
#       group_by_annotationUI(ns),
#       textInput(ns("new_group"), "New Group"),
#       actionButton(ns("add_group"), label = "Add Group"),
#       textInput(ns("new_annotation"), "New Annotation"),
#       actionButton(ns("add_annotation"), label = "Add annotation"),
#       verbatimTextOutput(ns("selectedData"))
#     )
#   )
# }

# plotlyServer <- function(id, myReactives) {
#   moduleServer(id, function(input, output, session) {
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)
#     legend <- reactive(input$legend)


#     # update group by
#     observeEvent(myReactives$seurat_object, {
#       if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         myReactives <- group_col(myReactives)
#         if ("TCR" %in% names(myReactives$seurat_object@meta.data)) {
#           myReactives$group_cols[["TCR"]] <- "TCR"
#           myReactives$group_cols[["TCR_count_per_sample"]] <- "TCR_count_per_sample"
#           myReactives$group_cols[["TCR_expand"]] <- "TCR_expand"
#         }

#         if ("BCR" %in% names(myReactives$seurat_object@meta.data)) {
#           myReactives$group_cols[["BCR"]] <- "BCR"
#           myReactives$group_cols[["BCR_count_per_sample"]] <- "BCR_count_per_sample"
#           myReactives$group_cols[["BCR_expand"]] <- "BCR_expand"
#         }

#         updateRadioButtons(session, "group_by", choices = myReactives$group_cols)
#         updateRadioButtons(session, "group_by_annotation", choices = myReactives$group_cols)
#       }
#     })

#     observeEvent(input$add_group, {
#       myReactives$group_cols[[input$new_group]] <- input$new_group
#       updateRadioButtons(session, "group_by", choices = myReactives$group_cols)
#       updateRadioButtons(session, "group_by_annotation", choices = myReactives$group_cols)
#       myReactives$seurat_object@meta.data[[input$new_group]] <- "none"
#     })

#     # df <- reactive({
#     #   df <- read.csv("/user/ifrec/mishikawa/SiCR/df.csv")
#     #   mutate(df, new_column = NA)
#     # })


#     df <- reactive({
#       req(myReactives$seurat_object)
#       if (input$reduction == "umap") {
#         coodinate <- myReactives$seurat_object[["umap"]]@cell.embeddings
#       } else if (input$reduction == "tsne") {
#         coodinate <- myReactives$seurat_object[["tsne"]]@cell.embeddings
#       }
#       coodinate_named <- as.data.frame(coodinate)
#       names(coodinate_named) <- c("x", "y")
#       df <- cbind(myReactives$seurat_object@meta.data, coodinate_named)
#       mutate(df, new_column = NA)
#     })


#     output$plot <- renderPlotly({
#       plot_ly(
#         data = df(),
#         x = ~x,
#         y = ~y,
#         type = "scatter",
#         mode = "markers",
#         color = ~ .data[[input$group_by]],
#         marker = list(size = input$point_size)
#         #    marker = list(size = input$point_size, color = ~.data[[input$group_by]]),
#       ) %>%
#         layout(width = input$plot_width, height = input$plot_height)
#     })


#     # observeEvent(input$button, {
#     #   # ボタンが押されたときに実行されるコード

#     #   # 選択されたデータポイントの情報を取得
#     #   s <- event_data("plotly_selected")
#     #   if (!is.null(s)) {
#     #     result <- left_join(s, df(), by = c("x", "y"))

#     #     # df()のresult$barcodeと同じ行のnew_columnに値を設定
#     #     df()$new_column[df()$barcode == result$barcode] <- "your_value"
#     #   }
#     # })
#     # selected_barcode <- reactive({
#     #   req(s)
#     #   s <- event_data('plotly_selected')
#     #   if(!is.null(s)){
#     #     result <- left_join(s, df(), by = c("x", "y"))
#     #     result$barcode
#     #   }
#     # })

#     # df <- reactive({
#     #   if(!is.null(s)){
#     #     df() %>% dplyr::filter(barcode %in% selected_barcode())
#     #   }
#     # })

#     # output$selectedData <- renderPrint({
#     #   selected_barcode()
#     # })

#     observeEvent(input$add_annotation, {
#       group_by_annotation <- input$group_by_annotation
#       myReactives$seurat_object@meta.data <- myReactives$seurat_object@meta.data %>%
#         mutate(group_by_annotation = case_when(
#           barcode %in% myReactives$selected_barcodes ~ input$new_annotation,
#           TRUE ~ input$group_by_annotation
#         ))
#     })

#     output$selectedData <- renderPrint({
#       # 選択されたデータポイントの情報を取得
#       s <- event_data("plotly_selected")
#       if (is.null(s)) {
#         "細胞を選択してください。"
#       } else {
#         myReactives$selected_barcodes <- left_join(s, df(), by = c("x", "y")) %>% .$barcode
#         return(myReactives$selected_barcodes)
#         # df <- df() %>% mutate(new_column = case_when(
#         #   barcode %in% barcodes ~ "your_value",
#         #   TRUE ~ NA
#         # ))
#         # df
#         # df()のresult$barcodeと同じ行のnew_columnに値を設定
#         #    df() <- df() %>% dplyr::filter(barcode %in% result$barcode)
#         #    result$barcode
#         #      print(s)
#         #      selected_cells <- sample_data[s$pointNumber + 1, ]
#         #      paste("選択された細胞のタイプ:", toString(unique(selected_cells$CellType)))
#       }
#     })

#     output$table <- renderDT(df())

#     output$download_marker <- downloadHandler(
#       filename = function() {
#         paste("markers-", Sys.Date(), ".csv", sep = "")
#       },
#       content = function(file) {
#         data <- df()
#         write.csv(data, file, row.names = FALSE)
#       }
#     )


#     # render_plot(output, "plot", reactive({
#     #   myReactives$dimplot
#     # }), plot_width, plot_height)
#     # setupDownloadPlotHandler(output, input, reactive({
#     #   myReactives$dimplot
#     # }))
#   })
# }



# # plotlyUI <- function(id){
# #   ns <- NS(id)
# #   sidebarLayout(
# #     sidebarPanel(
# #       reduction_methodUI(ns),
# #       group_byUI(ns),
# #       split_byUI(ns),
# #       point_sizeUI(ns),
# #       label_sizeUI(ns),

# #       uiOutput(ns('ncol_ui')),
# #       radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
# #       sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
# #       sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
# #       downloadButton(ns("downloadPlot"), "Download Plot as PDF")

# #     ),
# #     mainPanel(
# #       plotlyOutput(ns('plot')),
# #        verbatimTextOutput("selectedData")

# #     )
# #   )
# # }

# # plotlyServer <- function(id, myReactives){
# #   moduleServer(id, function(input, output, session){
# #     plot_width <- reactive(input$plot_width)
# #     plot_height <- reactive(input$plot_height)
# #     legend <- reactive(input$legend)

# #     observeEvent(myReactives$seurat_object,{
# #       if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
# #         group_cols <- update_group_by2(myReactives)
# #         updateSelectInput(session, "group_by", choices = group_cols)
# #       }
# #     })


# #   observeEvent(myReactives$seurat_object,{
# #     update_split_by(myReactives, session)
# #   })

# #   output$ncol_ui <- renderUI({
# #           if (input$split_by != 'none') {
# #             numericInput(session$ns('feature_column'), label = 'Number of columns', min = 1, value = 1)
# #           }
# #         })



# #     # Plot
# #     observe({
# #       if(!is.null(myReactives$seurat_object)){
# #         if(input$split_by == "none"){
# #           myReactives$dimplot <- DimPlot(
# #             myReactives$seurat_object,
# #             reduction = input$reduction,
# #             label = TRUE,
# #             pt.size = input$point_size,
# #             group.by = input$group_by,
# #             label.size = input$label_size
# #           ) + theme(legend.position = legend()) # nolint
# #         }
# #         else {
# #           myReactives$dimplot <- DimPlot(
# #             myReactives$seurat_object,
# #             reduction = input$reduction,
# #             label = TRUE,
# #             pt.size = input$point_size,
# #             group.by = input$group_by,
# #             split.by = input$split_by,
# #             label.size = input$label_size,
# #             ncol = input$feature_column
# #           ) + theme(legend.position = legend()) # nolint
# #         }
# #       }
# #     })

# #   output$plot <- renderPlotly({
# #   req(myReactives$dimplot)
# #   plot <- ggplotly(myReactives$dimplot) %>% layout(width = plot_width(), height = plot_height(),
# #     legend = list(
# #       font = list(size = 10), # フォントサイズの調整
# #         tracegroupgap = 1 # グループ間のスペースの調整
# #     )
# #   )
# #   plot
# # })
# #   output$selectedData <- renderPrint({
# #     # 選択されたデータポイントの情報を取得
# #     s <- event_data("plotly_selected")
# #     if (is.null(s)) {
# #       "細胞を選択してください。"
# #     } else {
# #       print(s)
# # #      selected_cells <- sample_data[s$pointNumber + 1, ]
# # #      paste("選択された細胞のタイプ:", toString(unique(selected_cells$CellType)))
# #     }
# #   })

# #     setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
# #   })
# # }
