volcano_plotUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        group_byUI(ns),
        ident1UI(ns),
        ident2UI(ns),
        point_sizeUI(ns),
        visibilityUI(ns),
        xinterceptUI(ns),
        yinterceptUI(ns),
        number_gene_labelUI(ns),
        runbuttonUI(ns),
        uiOutput(ns('marker')) # ここでUIの出力を定義
    ),
    mainPanel(
      DTOutput(ns('df')),
      plotOutput(ns('plot'))
    )
  )
}

volcano_plotServer <- function(id, myReactives) {
    moduleServer(id, function(input, output, session) {

        plot_width <- reactive(input$plot_width)
        plot_height <- reactive(input$plot_height)
        legend <- reactive(input$legend)

#練習用
    # markers <- reactive({
    #     read.csv('/user/ifrec/mishikawa/SiCR/markers.csv', row.names = 1)
    # })

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "group_by", choices = group_cols)
      }
    })

    observeEvent(myReactives$seurat_object, {
      if (!is.null(myReactives$seurat_object)) {
        updateSelectInput(session, "ident1", choices = c(sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
        updateSelectInput(session, "ident2", choices = c("Other groups" = "all", sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))))
      }
    })

    markers <- eventReactive(input$run,{
        Idents(myReactives$seurat_object) <- input$group_by
        if (input$ident2 == 'all'){
            FindMarkers(myReactives$seurat_object, 
                ident.1 = input$ident1,
                logfc.threshold = 0,
                min.pct = 0.01)
        }
        else {
        FindMarkers(myReactives$seurat_object, 
            ident.1 = input$ident1, 
            ident.2 = input$ident2,
            logfc.threshold = 0,
            min.pct = 0.01)
        }
       })

    observe({
        req(markers())
        write.csv(markers(), 'markers.csv')
    })


    output$df <- renderDT(markers(), filter = 'top')
    
    output$plot <- renderPlot({
        req(markers())
        volcano(markers(), alpha = input$visibility, size = input$point_size, xintercept_low = input$xintercept[1], xintercept_high = input$xintercept[2], yintercept = input$yintercept, pct = 0.01, avg_log2FC = 0.25, gene_label = "significance", number_gene_label = input$number_gene_label)
    })

    })
}