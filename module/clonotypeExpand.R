
clonotypeExpandUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(ns("heat_or_bar"), "Heatmap or barplot?", choices = list("heatmap" = "heatmap", "barplot" = "barplot"), selected = "barplot"),
      radioButtons(ns("count_or_percent"), "Use cell count or percent?", choices = list("count" = "count", "percent" = "percent"), selected = "count"),
      selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
      selectInput(ns("focus_group"), "Show top 20 in this group", choices = ""),
      selectInput(ns("palette"), "Color palette", choices = ""),
      checkboxInput(ns("legend"), "Show legend", value = TRUE),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns("clonotype_expand_plot")),
      downloadButton(ns("download_data"), "Download data (.csv)"),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}




clonotypeExpandServer <- function(id, myReactives, chain = "TCR_TRB_raw_clonotype_id") {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)


    observe({
    if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
      # 既存の選択肢を設定
      group_cols <- list("sample" = "sample", "seurat_clusters" = "seurat_clusters")
      # meta_dataの各列について、選択肢を追加
      meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
     for(col in meta_data_cols) {
        group_cols[[col]] <- col
      }
    updateSelectInput(session, "group_by", choices = group_cols)
    }
    })

    observe({
    if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
      # focus_group の選択肢を動的に更新
      updateSelectInput(session, "focus_group", choices = unique(myReactives$seurat_object@meta.data[input$group_by]))
    }
    })

    observeEvent(input$heat_or_bar, {
      if (input$heat_or_bar == "barplot") {
        updateSelectInput(session, "palette", choices = palette_list)
      } else if (input$heat_or_bar == "heatmap") {
        updateSelectInput(session, "palette", choices = list("Greys"="grey40", "Blues"="#2171B5", "Oranges"="#F16913", "Purples"="#807DBA"))
      }
    })


    clonotypeTally <- reactive({
      my_tally(myReactives$seurat_object@meta.data, group_by = input$group_by, x = chain)
    })

    # Extract top20
    top <- 20

    clonotypeTallyTop <- reactive({
      # TopのClonotypeをとってくる
      top_clonotype_ids <- clonotypeTally() %>%
        filter(if_all(all_of(input$group_by), ~ . == input$focus_group)) %>%
        top_n(top, count) %>% # this will result in more then 20 rows if there are ties
          head(top) %>%
            pull(chain)
      df <- clonotypeTally() %>%
        dplyr::filter(!!sym(chain) %in% top_clonotype_ids) %>%
        mutate(chain = factor(chain, levels = top_clonotype_ids)) # To order from top1 to 20 in the graph
      print(df)
      return(df)
    })
    
    # Get plot
    clonotypeExpandPlot <- reactive({
      
      # heatmap
      if(input$heat_or_bar == "heatmap") {
          if(input$count_or_percent == "count") {
            round_n <- 0
          } else {
            round_n <- 1
          }
        g <- my_heatmap(
          clonotypeTallyTop(),
          x = chain,
          y = input$group_by,
          fill = input$count_or_percent,
          round = round_n,
          legend_position = input$legend,
          high_color = input$palette,
          color_limit = NA
        )
      }
      
      # barplot
      if(input$heat_or_bar == "barplot") {
        g <- my_barplot(
          clonotypeTallyTop(),
          x = chain,
          y = input$count_or_percent,
          fill = input$group_by,
          position = "dodge",
          legend_position = input$legend,
          flip = FALSE,
          palette = input$palette
        )
      }
      
      return(g)
      
    })
    
    # output
    output$clonotype_expand_plot <- renderPlot(
      clonotypeExpandPlot(),
      width  = plot_width,
      height = plot_height
    )
    
    output$download_data <- downloadHandler(
      filename = function() {"clonotype_expand.csv"},
      content = function(file) {
        write_csv(clonotypeTally(), file)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {"clonotype_expand.pdf"},
      content = function(file) {
        ggsave(file, plot = clonotypeExpandPlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
      }
    )
    
  })
}