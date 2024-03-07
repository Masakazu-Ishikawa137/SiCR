dimensional_plot2UI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      selectInput(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample")),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 5, step = 1),
      numericInput(ns('feature_column'), label = 'Number of columns', min = 1, value = 1),
      radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF"),
      textInput(ns("gene"), "Enter feature (gene) names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19"),
      downloadButton(ns('available_feature'), "You can download available gene name")
    ),
    mainPanel(
      plotOutput(ns('plot'))
    )
  )
}

dimensional_plot2Server <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)
    ncol <- reactive(input$feature_column)
    split_by <- reactive(input$split_by)
    label_size <- reactive(input$label_size)

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by2(myReactives)
        group_cols[["Gene"]] <- "Gene"
        updateSelectInput(session, "group_by", choices = group_cols)
      }
    })

    observe({
      if(!is.null(myReactives$seurat_object)){
        if(input$group_by != "Gene"){
          myReactives$dimplot <- dimplot(myReactives, input, output, session)
        }
      }
    })
    render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
    setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))

  })
}


# dimensional_plot2UI <- function(id) {
#   ns <- NS(id)
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons(ns('reduction'), 'Dimensional Reduction Method', choices = c('UMAP' = 'umap', 'T-SNE' = 'tsne'), selected = 'umap'),
#       selectInput(ns("group_by"), "Colored by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample")),
#       textInput(ns("gene"), "Enter feature (gene) names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19"),
#       uiOutput(ns('ncol_ui')),
#       radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
#       sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01),
#       uiOutput(ns('label_size')),
#       uiOutput(ns('split_by_ui')),
#       sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
#       sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
#       downloadButton(ns("downloadPlot"), "Download Plot as PDF") 
#     ),
#     mainPanel(
#       plotOutput(ns('plot'))
#     )
#   )
# }

# dimensional_plot2Server <- function(id, myReactives) {
#   moduleServer(id, function(input, output, session) {
#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)

#     observeEvent(myReactives$seurat_object, {
#       if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
#         group_cols <- update_group_by2(myReactives)
#         group_cols[["Gene"]] <- "Gene"
#         updateSelectInput(session, "group_by", choices = group_cols)
#       }
#     })

#     observeEvent(input$group_by, {
#       if (input$group_by != "Gene" && !is.null(myReactives$seurat_object)) {
#         split_group <- update_split_by_new(myReactives)
#         output$split_by_ui <- renderUI({
#           selectInput(session$ns('split_by'), "Split by", choices = split_group)
#         })
#       } else {
#         output$split_by_ui <- renderUI({ NULL }) # 非表示にするためにNULLを返す
#       }
#     })

#     # `observe`を`observeEvent`に変更して、`input$group_by`の変更をトリガーとします。
#     # これにより、不要な再評価を防ぎ、エラーの原因となる条件をより明確に扱えます。
#     observeEvent(input$group_by, {
#       if (!is.null(myReactives$seurat_object)) {
#         if(input$group_by != "Gene"){
#           myReactives$dimplot <- dimplot(myReactives, input, output, session)
#         } else {
#           myReactives$dimplot <- featureplot(myReactives, input, output, session)
#         }
#       }
#     })

#     render_plot(output, 'plot', reactive({ myReactives$dimplot }), plot_width, plot_height)
#     setupDownloadPlotHandler(output, input, reactive({ myReactives$dimplot }))
#   })
# }