library("alakazam")

alphaDiversityUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      group_byUI(ns),
      checkboxInput(ns("sd"), "Show SD", value = TRUE),
      legend_placeUI(ns),
      plot_size_and_download_pdfUI(ns),
    ),
    mainPanel(
      plotOutput(ns("plot")),
      #      downloadButton(ns("download_data"), "Download data (.csv)"),
      #      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}




alphaDiversityServer <- function(id, myReactives, chain = "TCR_TRB_raw_clonotype_id") {
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

    # observe({
    #   if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
    #     # 既存の選択肢を設定
    #     group_cols <- list("sample" = "sample", "seurat_clusters" = "seurat_clusters")
    #     # meta_dataの各列について、選択肢を追加
    #     meta_data_cols <- names(myReactives$seurat_object@misc$meta_data)
    #    for(col in meta_data_cols) {
    #       group_cols[[col]] <- col
    #     }
    #   updateSelectInput(session, "group_by", choices = group_cols)
    #   }
    # })   # get alpha_diversity

    alphaDiversity <- reactive({
      if (is.null(myReactives$seurat_object) || is.null(myReactives$seurat_object@meta.data)) {
        return(NULL) # エラー回避のためにNULLを返す
      }
      data <- myReactives$seurat_object@meta.data %>%
        #        dplyr::select(starts_with("TCR_TRB")) %>%
        drop_na(any_of(c(chain, input$group_by)))
      alakazam::alphaDiversity(
        data,
        clone  = chain,
        group  = input$group_by,
        min_q  = 0,
        max_q  = 4,
        step_q = 1,
        nboot  = 100,
      )
    })


    observe({
      if (is.null(myReactives$seurat_object) || is.null(myReactives$seurat_object@meta.data)) {
        return(NULL) # エラー回避のためにNULLを返す
      }
      data <- myReactives$seurat_object@meta.data %>%
        #        dplyr::select(starts_with("TCR_TRB")) %>%
        drop_na(any_of(c(chain, input$group_by)))
      alphaDiversity <- alakazam::alphaDiversity(
        data,
        clone  = chain,
        group  = input$group_by,
        min_q  = 0,
        max_q  = 4,
        step_q = 1,
        nboot  = 100,
      )

      # plot
      g <- plot(alphaDiversity(), as.numeric(1)) +
        theme_classic() +
        theme(
          legend.position = legend(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      if (!input$sd) {
        g$layers <- g$layers[-1]
      }

      myReactives$alpha_diversity_plot <- g
    })

    render_plot(
      output,
      "plot",
      reactive({
        myReactives$alpha_diversity_plot
      }),
      reactive({
        input$plot_width
      }),
      reactive({
        input$plot_height
      })
    )


    setupDownloadPlotHandler(output, input, reactive({
      myReactives$alpha_diversity_plot
    }))


    # get plot

    # observe({
    #   req(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)
    #   g <- plot(alphaDiversity(), as.numeric(1)) +
    #     theme_classic() +
    #     theme(
    #       legend.position = legend(),
    #       axis.text.x  = element_text(angle = 45, hjust = 1)
    #     )
    #   # if (input$q == "all") {
    #   #   g <- plot(alphaDiversity())
    #   # } else {
    #   #   g <- plot(alphaDiversity(), as.numeric(input$q))
    #   # }
    #   if (!input$sd){
    #     g$layers <- g$layers[-1]
    #   }

    #   myReactives$alpha_diversity_plot <- g
    # })


    #     alphaDiversityPlot <- reactive({
    #       g <- plot(alphaDiversity(), as.numeric(1)) +
    #         theme_classic() +
    #         theme(
    #           legend.position = legend(),
    #           axis.text.x  = element_text(angle = 45, hjust = 1)
    #         )
    #       # if (input$q == "all") {
    #       #   g <- plot(alphaDiversity())
    #       # } else {
    #       #   g <- plot(alphaDiversity(), as.numeric(input$q))
    #       # }
    #       if (!input$sd){
    #         g$layers <- g$layers[-1]
    #       }

    #       return(g)
    # #      my_colors <- my_palette(length(unique(pull(alphaDiversity()@diversity, !!alphaDiversity()@group_by))), input$palette)
    #       # g <- g +
    #       #   labs(title=NULL) +
    #       #   scale_color_manual(values = my_colors) +
    #       #   scale_fill_manual(values = my_colors) +
    #       #   theme_classic() +
    #       #   theme(
    #       #     legend.position = legend(),
    #       #     axis.text.x  = element_text(angle = 45, hjust = 1)
    #       #   )

    #       # return(g)
    #     })

    # output plot
    # output$alpha_diversity_plot <- renderPlot(
    #   alphaDiversityPlot(),
    #   width  = plot_width,
    #   height = plot_height
    # )

    # output$download_data <- downloadHandler(
    #   filename = function() {"alpha_diversity.csv"},
    #   content = function(file) {
    #     write_csv(alphaDiversity()@diversity, file)
    #   }
    # )

    # output$download_plot <- downloadHandler(
    #   filename = function() {"alpha_diversity.pdf"},
    #   content = function(file) {
    #     ggsave(file, plot = alphaDiversityPlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
    #   }
    # )
  })
}


# alphaDiversityServer <- function(id, data, group_cols) {
#   moduleServer(id, function(input, output, session) {

#     plot_width <- reactive(input$plot_width)
#     plot_height <- reactive(input$plot_height)
#     legend <- reactive(ifelse(input$legend, "right", "none"))
#     observe(updateSelectInput(session, "group_by", choices = group_cols))

#     # get alpha_diversity
#     alphaDiversity <- reactive({
#       data <- data %>%
#         drop_na(any_of(c("raw_clonotype_id", input$group_by)))
#       alakazam::alphaDiversity(
#         data,
#         clone  = "raw_clonotype_id",
#         group  = input$group_by,
#         min_q  = 0,
#         max_q  = 4,
#         step_q = 1,
#         nboot  = 100,
#       )
#     })

#     # get plot
#     alphaDiversityPlot <- reactive({
#       if (input$q == "all") {
#         g <- plot(alphaDiversity())
#       } else {
#         g <- plot(alphaDiversity(), as.numeric(input$q))
#       }
#       if (!input$sd){
#         g$layers <- g$layers[-1]
#       }
#       my_colors <- my_palette(length(unique(pull(alphaDiversity()@diversity, !!alphaDiversity()@group_by))), input$palette)
#       g <- g +
#         labs(title=NULL) +
#         scale_color_manual(values = my_colors) +
#         scale_fill_manual(values = my_colors) +
#         theme_classic() +
#         theme(
#           legend.position = legend(),
#           axis.text.x  = element_text(angle = 45, hjust = 1)
#         )

#       return(g)
#     })

#     # output plot
#     output$alpha_diversity_plot <- renderPlot(
#       alphaDiversityPlot(),
#       width  = plot_width,
#       height = plot_height
#     )

#     output$download_data <- downloadHandler(
#       filename = function() {"alpha_diversity.csv"},
#       content = function(file) {
#         write_csv(alphaDiversity()@diversity, file)
#       }
#     )

#     output$download_plot <- downloadHandler(
#       filename = function() {"alpha_diversity.pdf"},
#       content = function(file) {
#         ggsave(file, plot = alphaDiversityPlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
#       }
#     )

#   })
# }
