quality_controlUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      sliderInput(ns("slider_nCount_RNA"), label = "cCount", min = 0, max = 100000, value = c(0, 100000)),
      sliderInput(ns("slider_nFeatures_RNA"), label = "nFeature", min = 0, max = 10000, value = c(0, 10000)),
      sliderInput(ns("slider_percent_mt"), label = "percent mt", min = 0, max = 100, value = 100),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 1, value = 0.1, step = 0.01),
      sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 900, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 900, step = 100),
      actionButton(ns('rerun'), label = "Rerun")
    ),
    mainPanel(
      plotOutput(ns("filter_plot")),
    )
  )
}

quality_controlServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)


    # Update input
    observeEvent(myReactives$seurat_object, {
      update_filter_slide(session, myReactives)
    })


    output$filter_plot <- renderPlot(
      {
        req(myReactives$seurat_object)
        quality_control_plot(myReactives, input)
      },
      width = plot_width,
      height = plot_height
    )

    observeEvent(input$rerun, {
    myReactives$seurat_object <- myseurat_normalize_umap_rerun(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
  })



    # observeEvent(myReactives$seurat_object, {
    #   if (!is.null(myReactives$seurat_object)) {
    #     group_cols <- update_group_by2(myReactives)
    #     current_selection <- input$group_by
    #     if (!current_selection %in% names(group_cols)) {
    #       current_selection <- names(group_cols)[1]
    #     }
    #     updateRadioButtons(session, "group_by", choices = group_cols, selected = current_selection)
    #   }
    # })


    # toListen <- reactive({
    #   list(myReactives$seurat_object, input$group_by)
    # })
    # observeEvent(toListen(),
    #   {
    #     # 新しいgroup_byに基づいてfocus_groupの選択肢を更新
    #     if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)) {
    #       myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    #       updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
    #     }
    #   },
    #   ignoreNULL = TRUE,
    #   ignoreInit = TRUE
    # )
    # observe({
    #   req(myReactives$seurat_object)
    #   myReactives$highlight_plot <- draw_highlight(myReactives, input, output, session)
    # })


    # render_plot(
    #   output,
    #   "highlight_plot",
    #   reactive({
    #     myReactives$highlight_plot
    #   }),
    #   reactive({
    #     input$plot_width
    #   }),
    #   reactive({
    #     input$plot_height
    #   })
    # )

    # setupDownloadPlotHandler(output, input, reactive({
    #   myReactives$highlight_plot
    # }))
  })
}




# Quality_controlUI <- function(id){
#   ns <- NS(id)
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput(ns('slider_nCount_RNA'), label = "cCount", min = 0, max = 100000, value = c(0, 100000)),
#       sliderInput(ns('slider_nFeatures_RNA'), label = "nFeature", min = 0, max = 10000, value = c(0, 10000)),
#       sliderInput(ns('slider_percent_mt'), label = "percent mt", min = 0, max = 100, value = 100),
#       sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 1, value = 0.1, step = 0.01),
#       sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 900, step = 100),
#       sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 900, step = 100),
#       reduction_methodUI(ns),
#       group_byUI(ns),
#       focus_groupUI(ns),
#       point_sizeUI(ns),
#       highlight_sizeUI(ns),
#       plot_size_and_download_pdfUI(ns),

#       downloadButton(ns("downloadPlot"), "Download Plot as PDF"),
#       actionButton(ns("quality_control_rerun"), "Re-run"),
#     ),
#     mainPanel(
#       textOutput(ns('rerun_started')),
#       plotOutput(ns('filter_plot')),
#       plotOutput(ns("highlight_plot"))

#     ),
#   )
# }
# Quality_controlServer <- function(id, myReactives){
#   moduleServer(id, function(input, output, session){
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)

#   # Update input
#   observeEvent(myReactives$seurat_object,
#                {
#                  seurat_object <- myReactives$seurat_object
#                  updateSliderInput(session, 'slider_nCount_RNA', min = min(seurat_object@meta.data$nCount_RNA), max = max(seurat_object@meta.data$nCount_RNA), value = c(min(seurat_object@meta.data$nCount_RNA), max(seurat_object@meta.data$nCount_RNA)))
#                  updateSliderInput(session, 'slider_nFeatures_RNA', min = min(seurat_object@meta.data$nFeature_RNA), max = max(seurat_object@meta.data$nFeature_RNA), value = c(min(seurat_object@meta.data$nFeature_RNA), max(seurat_object@meta.data$nFeature_RNA)))
#                  updateSliderInput(session, 'slider_percent_mt', min = 0, max = floor(max(seurat_object@meta.data$percent.mt)) + 1, value = max(seurat_object@meta.data$percent.mt))
#                  updateCheckboxGroupInput(session, 'subsetting', choices = sort(unique(seurat_object@meta.data$seurat_clusters)))
#                  output$change_name <- renderUI({
#                    map(as.character(sort(unique(seurat_object@meta.data$seurat_clusters))), ~ textInput(.x, .x))
#                  })
#                })

#   #Quality control
#   observe({
#     if (!is.null(myReactives$seurat_object)) {
#       myReactives$quality_sca <- myquality_control_scatterplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt, input$point_size)
#       myReactives$quality_vln <- myquality_control_violinplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
#     }
#   })

#   output$filter_plot <- renderPlot({
#       req(myReactives$seurat_object)
#       myReactives$quality_control_plot <- ggarrange(myReactives$quality_sca, myReactives$quality_vln, ncol = 1)
#       myReactives$quality_control_plot
#     },
#     width = plot_width, height = plot_height)

#     setupDownloadPlotHandler(output, input, reactive({ myReactives$quality_control_plot }))



#   observeEvent(input$quality_control_rerun, {
#     output$rerun_started <- renderText('Re-run started')
#     myReactives$seurat_object <- myseurat_normalize_umap_rerun(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
#   })
# })
# }
