marker_showUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("group_by"), "Group by", choices = c("seurat_clusters", "sample")),
      actionButton(ns("run"), "Run"),
      numericInput(ns("num"), label = h3("Numeric input"), value = 10),
      radioButtons(ns("choice"), "Choice", choices = list("positive", "negative", "both"), selected = "positive")
    ),
    mainPanel(
      tableOutput(ns("marker_table"))
    )
  )
}

marker_showServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object@misc$meta_data)) {
        meta_data_cols <- c("seurat_clusters", "sample", names(myReactives$seurat_object@misc$meta_data))
        # 直接辞書を作成
        updateSelectInput(session, "group_by", choices = setNames(meta_data_cols, meta_data_cols))
      }
    })

    observeEvent(input$run, {
      myReactives$marker_df <- FindAllMarkers(myReactives$seurat_object, group.by = input$group_by)
    })

    observe({
      if (!is.null(myReactives$marker_df)) {
        # マーカーテーブルのレンダリングを効率化
        output$marker_table <- renderTable({
          direction <- switch(input$choice,
                              positive = 1,
                              negative = -1,
                              both = 0)
          if (direction == 0) { # 両方の場合
            sorting_col <- abs(myReactives$marker_df$avg_log2FC)
          } else { # 正または負の場合
            sorting_col <- direction * myReactives$marker_df$avg_log2FC
          }
          myReactives$marker_df %>% 
            group_by(cluster) %>%
            arrange(desc(sorting_col)) %>%
            slice_head(n = input$num) %>%
            summarise(genes = paste(gene, collapse = ", "), .groups = 'drop')
        })
      }
    })
  })
}