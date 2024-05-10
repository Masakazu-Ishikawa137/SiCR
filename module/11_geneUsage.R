# gene usageはclonotypeは含めない
# csvも表示するようにする

geneUsageUI <- function(id, chain = "TCR") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      chain_selectionUI(ns, chain),
      gene_selectionUI(ns),
      group_byUI(ns),
      focus_groupUI(ns),
      legend_placeUI(ns),
      plot_size_and_download_pdfUI(ns),

      # radioButtons(ns("count_or_percent"), "Use cell count or percent?", choices = list("count" = "count", "percent" = "percent"), selected = "count"),
      # selectInput(ns("chain"), "Cain", choices = chain_choice),
      # selectInput(ns("gene"), "Gene", choices = ""),
      # selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
      # selectInput(ns("palette"), "Color palette", choices = palette_list),
      # radioButtons(ns("ver_or_hori"), "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
      # checkboxInput(ns("legend"), "Show legend", value = TRUE),
      # sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
      # sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      DTOutput(ns("table")),
      plotOutput(ns("plot")),
      plotOutput(ns("plot2")),

      # plotOutput(ns("gene_usage_plot")),
      # downloadButton(ns("download_data"), "Download data (.csv)"),
      # downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}



geneUsageServer <- function(id, myReactives, chain = "TCR") {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(input$legend)
    # plot_width <- commonSettings$plot_width
    # plot_height <- commonSettings$plot_height
    # legend <- commonSettings$legend

    # update group by
    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
      update_group_by(session, myReactives)
    })

    toListen <- reactive({
      list(myReactives$seurat_object, input$group_by)
    })
    # observeEvent(toListen(),
    #   {
    #     # 新しいgroup_byに基づいてfocus_groupの選択肢を更新
    #     if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)) {
    #       myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    #       updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = myReactives$new_choices)
    #     }
    #   },
    #   ignoreNULL = TRUE,
    #   ignoreInit = TRUE
    # )

    observeEvent(toListen(), {
      req(myReactives$seurat_object)
      myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
      updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = myReactives$new_choices)
    })

    input_lists <- reactive({
      list(input$chain, input$gene, input$group_by, input$focus_group)
    })



    observeEvent(input_lists(), {
      req(myReactives$seurat_object)
      if (input$gene != "v_j_gene") {
        gene_column <- paste0(chain, "_", input$chain, "_", input$gene)
        myReactives$gene_usage_df <- calculate_gene_usage(myReactives, gene_column, input$group_by)
        myReactives$gene_usage_df <- myReactives$gene_usage_df %>% dplyr::filter(.data[[input$group_by]] %in% input$focus_group)

        myReactives$gene_usage_plot <- myReactives$gene_usage_df %>%
          ggplot(aes(x = !!sym(gene_column), y = proportion, fill = !!sym(input$group_by))) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_classic() +
          scale_y_continuous(expand = c(0, 0)) +
          theme(
            legend.position = input$legend,
            axis.text.x = element_text(angle = 90)
          )
      } else if (input$gene == "v_j_gene") {
        metadata <- myReactives$seurat_object@meta.data
        v_gene_column <- paste0(chain, "_", input$chain, "_", "v_gene")
        print(v_gene_column)
        j_gene_column <- paste0(chain, "_", input$chain, "_", "j_gene")
        print(j_gene_column)
        metadata <- metadata %>% drop_na(v_gene_column, j_gene_column, input$group_by)
        metadata <- metadata %>% filter(.data[[input$group_by]] %in% input$focus_group)
        table <- as.data.frame(table(metadata[[v_gene_column]], metadata[[j_gene_column]]))
        names(table) <- c("from", "to", "value")
        table$sum <- sum(table$value)
        table <- table %>% mutate(proportion = value / sum)
        table <- table %>% select(from, to, proportion)
        myReactives$gene_usage_df <- table
        output$plot2 <- renderPlot({chordDiagram(myReactives$gene_usage_df)})
        #        output$plot2 <- renderPlot({chordDiagram(myReactives$gene_usage_df)})
        # myReactives$gene_usage_plot <- chordDiagram(myReactives$gene_usage_df)

        # myReactives$gene_usage_df <- calculate_gene_usage_v_j(myReactives, chain, input$gene, input$group_by, input$focus_group)
        # myReactives$gene_usage_plot <- chordDiagram(myReactives$gene_usage_df)
      }
    })

    
    output$table <- renderDT({
      myReactives$gene_usage_df
    })

    render_plot(
      output,
      "plot",
      reactive({
          myReactives$gene_usage_plot
      }),
      reactive({
        input$plot_width
      }),
      reactive({
        input$plot_height
      })
    )


    setupDownloadPlotHandler(output, input, reactive({
      myReactives$gene_usage_plot
    }))
  })
}
