highlightUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      reduction_methodUI(ns),
      group_byUI(ns),
      focus_groupUI(ns),
      point_sizeUI(ns),
      highlight_sizeUI(ns),
      plot_sizeUI(ns),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

highlightServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)

    # Seuratオブジェクトが更新されたときにgroup_byの選択肢を更新
    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by2(myReactives)
        current_selection <- input$group_by
        if (!current_selection %in% names(group_cols)) {
          current_selection <- names(group_cols)[1]
        }
        updateRadioButtons(session, "group_by", choices = group_cols, selected = current_selection)
      }
    })


    # toListen <- reactive({ list(myReactives$seurat_object, input$group_by) })
    # observeEvent(toListen(), {
    #   if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
    #     updateCheckboxGroupInput(session, "focus_group", selected = character(0))
    #     myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    #     updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
    #   }
    # })

    toListen <- reactive({ list(myReactives$seurat_object, input$group_by) })
    observeEvent(toListen(), {
  # 新しいgroup_byに基づいてfocus_groupの選択肢を更新
  if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)) {
    myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)


    observe({
      if (!is.null(myReactives$seurat_object) & !is.null(myReactives$new_choices)) {
        valid_focus_group_selection <- intersect(input$focus_group, myReactives$new_choices)
        if (length(valid_focus_group_selection) == length(input$focus_group)) {
          Idents(myReactives$seurat_object) <- input$group_by
          subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
          myReactives$highlight_plot <- DimPlot(
            myReactives$seurat_object,
            reduction = input$reduction,
            cells.highlight = subject,
            cols.highlight = "darkblue",
            sizes.highlight = input$highlight_size,
            cols = "grey",
            pt.size = input$point_size,
          ) + theme(legend.position = input$legend)
        }
      }
    })


    render_plot(output, 'plot', reactive({ myReactives$highlight_plot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$highlight_plot }))
  })
}


# highlightServer <- function(id, myReactives) {
#   moduleServer(id, function(input, output, session) {
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)
#     legend <- reactive(input$legend)

# #以下の修正では、Seuratオブジェクトが更新されたときにgroup_byの選択肢を更新しつつ、現在の選択をできるだけ保持するようにしています。また、選択肢が更新された際に、現在の選択が新しい選択肢に存在しない場合にのみデフォルト値（または他の適切な値）にリセットされるようにしています。

# observeEvent(myReactives$seurat_object, {
#   if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#     group_cols <- update_group_by2(myReactives)
#     current_selection <- input$group_by
#     if (!current_selection %in% names(group_cols)) {
#       current_selection <- names(group_cols)[1] # デフォルトの選択肢（ここでは最初の選択肢）に設定
#     }
#     updateRadioButtons(session, "group_by", choices = group_cols, selected = current_selection)
#   }
# })

#     # input$group_by の選択もしくはSeurat objectが変更されたとき、に input$focus_group をリセット
#     toListen <- reactive({ list(myReactives$seurat_object, input$group_by) })
#     observeEvent(toListen(), {
#       if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         updateCheckboxGroupInput(session, "focus_group", selected = character(0))
#         new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
#         updateCheckboxGroupInput(session, "focus_group", choices = new_choices, selected = character(0))
#       }
#     })

#     # Plot
#     observe({
#       if (!is.null(myReactives$seurat_object)) {
# #      if (!is.null(myReactives$seurat_object) & !is.null(myReactives$new_choices)) {
# #        valid_focus_group_selection <- intersect(input$focus_group, myReactives$new_choices)
# #        if (length(valid_focus_group_selection) == length(input$focus_group)) {
#           Idents(myReactives$seurat_object) <- input$group_by
#           subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
#           myReactives$highlight_plot <- DimPlot(
#             myReactives$seurat_object,
#             reduction = input$reduction,
#             cells.highlight = subject,
#             cols.highlight = "darkblue",
#             sizes.highlight = input$highlight_size,
#             cols = "grey",
#             pt.size = input$point_size,
#           ) + theme(legend.position = input$legend)
#         }
#     })
#     render_plot(output, 'plot', reactive({ myReactives$highlight_plot }), plot_width, plot_height)
#     setupDownloadPlotHandler(output, input, reactive({ myReactives$highlight_plot }))
#   })
# }

    # # group_by の選択が変更されたときに focus_group の選択をリセット
    # observeEvent(input$group_by, {
    #   if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
    #     updateCheckboxGroupInput(session, "focus_group", selected = character(0))
    #     new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    #     updateCheckboxGroupInput(session, "focus_group", choices = new_choices)
    #   }
    # }, ignoreNULL = FALSE)

# observe({
#   if (!is.null(myReactives$seurat_object)) {
#     # 利用可能なアイデンティティを取得
#     available_idents <- levels(Idents(myReactives$seurat_object))
#     # focus_groupの選択肢が有効かどうかを確認
#     valid_focus_group_selection <- input$focus_group %in% available_idents
    
#     if (all(valid_focus_group_selection)) {
#       Idents(myReactives$seurat_object) <- input$group_by
#       subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
#       myReactives$highlight_plot <- DimPlot(
#         myReactives$seurat_object,
#         reduction = input$reduction,
#         cells.highlight = subject,
#         cols.highlight = "darkblue",
#         sizes.highlight = input$highlight_size,
#         cols = "grey",
#         pt.size = input$point_size,
#       ) + theme(legend.position = input$legend)
#     } else {
#       myReactives$highlight_plot <- DimPlot(
#         myReactives$seurat_object,
#         reduction = input$reduction,
# #        cells.highlight = subject,
# #        cols.highlight = "darkblue",
# #        sizes.highlight = input$highlight_size,
#         cols = "grey",
#         pt.size = input$point_size,
#       ) + theme(legend.position = input$legend)
#     }
#   }
# })


    # Plotの生成
    # observe({
    #   if (!is.null(myReactives$seurat_object)) {
    #     Idents(myReactives$seurat_object) <- input$group_by
    #     subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
    #     myReactives$highlight_plot <- DimPlot(
    #       myReactives$seurat_object,
    #       reduction = input$reduction,
    #       cells.highlight = subject,
    #       cols.highlight = "darkblue",
    #       sizes.highlight = input$highlight_size,
    #       cols = "grey",
    #       pt.size = input$point_size,
    #     ) + theme(legend.position = input$legend)
    #   }
    # })