barplotUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(),
        mainPanel(
            plotOutput(ns('plot'))
        )
    )
}

barplotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    output$plot <- renderPlot({
      # seurat_objectがNULLでないことを確認
      req(!is.null(myReactives$seurat_object))
      
      # プロットのレンダリング
      myReactives$seurat_object@meta.data %>%
        ggplot(aes(x = sample, y = ..count.., fill = as.factor(seurat_clusters))) +
        geom_bar(stat = "count", position = "stack") +
        theme_classic() +
        scale_y_continuous(expand = c(0,0)) +
        labs(fill = "Seurat Clusters", y = "Count", x = "Sample")
    })
  })
}