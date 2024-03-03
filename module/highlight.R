# highlightUI <- function(id){
#   ns <- NS(id)
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
#       selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
#       checkboxGroupInput(ns("focus_group"), "Show top 20 in this group", choices = "", inline = TRUE),
#       sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10,  value = 0.1, step = 0.01),
#       sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
#       sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
#       downloadButton(ns("downloadPlot"), "Download Plot as PDF") 
#     ),
#     mainPanel(
#       plotOutput(ns('plot'))
#     )
#   )
# }

# # Server 部分の定義
# highlightServer <- function(id, myReactives){
#   moduleServer(id, function(input, output, session){
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)
    
#     observeEvent(myReactives$seurat_object,{
#       if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         group_cols <- update_group_by2(myReactives)
#         updateSelectInput(session, "group_by", choices = group_cols)
#       }
#     })

#     # input$group_by の選択が変更されたときに input$focus_group をリセット
#     observeEvent(input$group_by, {
#       if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         new_choices <- unique(myReactives$seurat_object@meta.data[[input$group_by]])
#         updateCheckboxGroupInput(session, "focus_group", choices = new_choices, selected = character(0))
#       }
#     })

#     # Plot
#     observe({
#       if(!is.null(myReactives$seurat_object)){
#         Idents(myReactives$seurat_object) <- input$group_by
#         subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
#         myReactives$dimplot <- DimPlot(
#           myReactives$seurat_object,
#           reduction = input$reduction,
#           cells.highlight = subject,
#           cols.highlight = "darkblue",
#           cols = "grey",
#           pt.size = input$point_size,
#           group.by = input$group_by,
#         ) + theme(legend.position = "none")
#       }
#     })

#     output$plot <- renderPlot({
#       myReactives$dimplot + theme(plot.title = element_text(size = 40, face = "bold"), 
#                                    legend.position = "none")
#     }, width = function() { plot_width() }, height = function() { plot_height() })
#   })
# }



# #p1 <- DimPlot(data, group.by = "condition2", cells.highlight = disease, cols.highlight = "darkblue", cols = "grey") + NoLegend() + ggtitle("disease") + theme(plot.title = element_text(size = 40, face = "bold"))
# #p2 <- DimPlot(data, group.by = "condition2", cells.highlight = healthy, cols.highlight = "darkblue", cols = "grey") + NoLegend() + ggtitle("healthy") + theme(plot.title = element_text(size = 40, face = "bold"))



highlightUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      checkboxGroupInput(ns("focus_group"), "Show top 20 in this group", choices = "", inline = TRUE),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10,  value = 0.1, step = 0.01),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF") 

    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

highlightServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)


#     observeEvent(myReactives$seurat_object, {
#   if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#     group_cols <- update_group_by2(myReactives) # これは、利用可能なグループの列を更新するための関数の仮の名前です
#     current_selection <- input$group_by
#     # 利用可能な選択肢の中に現在の選択が存在するか確認し、そうでない場合はデフォルト値（または適切な値）を使用
#     if (!current_selection %in% names(group_cols)) {
#       current_selection <- "seurat_clusters" # ここではデフォルト値を設定しています
#     }
#     updateSelectInput(session, "group_by", choices = group_cols, selected = current_selection)
#   }
# })
    
    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by2(myReactives)
        current_selection <- input$group_by
    if (current_selection %in% names(group_cols)) {
      # 現在の選択を保持
      updateSelectInput(session, "group_by", choices = group_cols, selected = current_selection)
    } else {
      # 現在の選択が選択肢にない場合、デフォルトまたは最初の選択肢を設定
      updateSelectInput(session, "group_by", choices = group_cols, selected = names(group_cols)[1])
    }      }
    })

toListen <- reactive({list(myReactives$seurat_object,input$group_by) })

    # input$group_by の選択が変更されたときに input$focus_group をリセット
    observeEvent(toListen(), {
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
        updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = NULL)
      }
    })


    # observe({
    # if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
    #   # focus_group の選択肢を動的に更新
    # updateCheckboxGroupInput(session, "focus_group", choices = unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    # }
    # })


    # Plot
    observe({
      if(!is.null(myReactives$seurat_object) & !is.null(myReactives$new_choices)){
        valid_focus_group_selection <- intersect(input$focus_group, myReactives$new_choices)
        if (length(valid_focus_group_selection) == length(input$focus_group)){
          Idents(myReactives$seurat_object) <- input$group_by
          subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
          myReactives$dimplot <- DimPlot(
            myReactives$seurat_object,
            reduction = input$reduction,
            cells.highlight = subject,
            cols.highlight = "darkblue",
            cols = "grey",
            pt.size = input$point_size,
            group.by = input$group_by,
          ) + theme(legend.position = input$legend)
        }
      }
    })
    render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
  })
}