library("alakazam")

clonalAbundanceUI <- function(id, chain = "TCR") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      type_plot_clonotype(ns),
      chain_selectionUI(ns, chain),
      clonotype_selectionUI(ns),
      group_byUI(ns),
      focus_groupUI(ns),
      legend_placeUI(ns),
      plot_size_and_download_pdfUI(ns),
    ),
    mainPanel(
      DTOutput(ns("df")),
      plotOutput(ns("plot")),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}


clonalAbundanceServer <- function(id, myReactives, chain = "TCR") {
  moduleServer(id, function(input, output, session) {
    # update group_by
    observeEvent(myReactives$seurat_object, {
      req(myReactives$seurat_object)
      update_group_by(session, myReactives)
    })

    toListen <- reactive({
      list(myReactives$seurat_object, input$group_by)
    })

    observeEvent(toListen(), {
      req(myReactives$seurat_object)
      myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
      updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = myReactives$new_choices)
    })

    gene_column <- reactive({
      paste0(chain, "_", input$chain, "_", input$clonotype)
    })

    # metadataを作る。Gene columnとGroupがNAを消して、focus_groupに含まれるものだけを抽出
    metadata <- reactive({
      req(myReactives$seurat_object)
      #      gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
      data <- myReactives$seurat_object@meta.data %>%
        drop_na(any_of(c(gene_column(), input$group_by))) %>%
        filter(.data[[input$group_by]] %in% input$focus_group)
      return(data)
    })


    observeEvent(metadata(), {
      req(metadata())
      output$df <- renderDT({
        metadata()
      })
      # if (input$type_plot_clonotype == "rank_abundance") {
      #   #        gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
      #   df <- alakazam::estimateAbundance(
      #     metadata(),
      #     clone  = paste0(chain, "_", input$chain, "_", input$clonotype),
      #     group  = input$group_by,
      #     ci     = 0.95,
      #     nboot  = 100
      #   )
      # }
    })
  })
}
#         output$df <- renderDT({
#           df()@abundance
#         })
#         render_plot(
#           output,
#           "plot",
#           reactive({
#             plot(df)
#           })
#         )
#      }

#   df <- eventReactive(metadata(), {
#     req(metadata())
#     gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#     alakazam::estimateAbundance(
#       metadata(),
#       clone  = gene_column,
#       grxitoup  = input$group_by,
#       ci     = 0.95,
#       nboot  = 100
#     )
#   })

#   output$df <- renderDT({
#     df()@abundance
#   })

#   render_plot(
#     output,
#     "plot",
#     reactive({
#       plot(df())
#     })
#   )
# }

# df <- eventReactive(metadata(), {
#   req(metadata())
#   if (input$type_plot_clonotype == "rank_abundance") {
#     gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#     alakazam::estimateAbundance(
#       metadata(),
#       clone  = gene_column,
#       group  = input$group_by,
#       ci     = 0.95,
#       nboot  = 100
#     )
#   } else if (input$type_plot_clonotype == "percent_of_unique_clonotype") {
#     gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#     metadata <- metadata() %>%
#       dplyr::group_by(.data[[input$group_by]])
#     unique <- metadata %>%
#       dplyr::summarize(n = n_distinct(.data[[gene_column]]))
#     print(unique)
#     total <- metadata %>%
#       dplyr::summarize(n = n())
#     print(total)
#     df <- dplyr::left_join(unique, total)
#     print(df)
#     return(df)
#   }
# })

# output$df <- renderDT({
#   if (input$type_plot_clonotype == "rank_abundance") {
#     df()@abundance
#   } else if (input$type_plot_clonotype == "percent_of_unique_clonotype") {
#     df()
#   }
# })

# render_plot(
#   output,
#   "plot",
#   reactive({
#     if (input$type_plot_clonotype == "rank_abundance") {
#       plot(df())
#     }
#   }),
#   reactive({
#     input$plot_width
#   }),
#   reactive({
#     input$plot_height
#   })
# )



# # # これはOK
#     clonalAbundance <- reactive({
#       req(myReactives$seurat_object)
#       gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#       data <- myReactives$seurat_object@meta.data %>%
#         drop_na(any_of(c(gene_column, input$group_by))) %>% filter(.data[[input$group_by]] %in% input$focus_group)
#       alakazam::estimateAbundance(
#         data,
#         clone  = gene_column,
#         group  = input$group_by,
#         ci     = 0.95,
#         nboot  = 100
#       )
#     })

# # get plot
# clonalAbundancePlot <- reactive({
#   plot(clonalAbundance())
# })




# metadata <- reactive({
#   req(myReactives$seurat_object)
#   gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#   data <- myReactives$seurat_object@meta.data %>%
#     drop_na(any_of(c(gene_column, input$group_by))) %>%
#     dplyr::filter(.data[[input$group_by]] %in% input$focus_group)
#   return(data)
# })

# observeEvent(metadata(),{
#   req(metadata())
#   if (input$type_plot_clonotype == 'rank_abundance'){
#   gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)
#   df <- alakazam::estimateAbundance(
#     metadata(),
#     clone  = metadata()[[gene_column]],
#     group  = metadata()[[input$group_by]],
#     ci     = 0.95,
#     nboot  = 100
#   )
#   print(df)
# #   myReactives$clonotype_abundance_plot <- plot(df)
# }
# })


# # get clonal_abundance
# clonalAbundance <- reactive({

#   alakazam::estimateAbundance(
#     data,
#     clone  = gene_column,
#     group  = input$group_by,
#     ci     = 0.95,
#     nboot  = 100
#   )
# })

# output$df <- renderDT({
#   clonalAbundance()@abundance
# })

# render_plot(
#   output,
#   "plot",
#   reactive({
#     clonalAbundancePlot()
#   }),
#   reactive({
#     input$plot_width
#   }),
#   reactive({
#     input$plot_height
#   })
# )


# setupDownloadPlotHandler(output, input, reactive({
#   myReactives$clonotype_abundance_plot
# }))


# # get plot
# clonalAbundancePlot <- reactive({
#   g <- plot(clonalAbundance())
#   if (!input$sd){
#     g$layers <- g$layers[-1]
#   }
#   my_colors <- my_palette(length(unique(pull(clonalAbundance()@abundance, !!clonalAbundance()@group_by))), input$palette)
#   g <- g +
#     labs(title=NULL) +
#     scale_color_manual(values = my_colors) +
#     scale_fill_manual(values = my_colors) +
#     theme_classic() +
#     theme(legend.position = legend())

#   return(g)
# })

# # output plot
# output$clonal_abundance_plot <- renderPlot(
#   clonalAbundancePlot(),
#   width  = plot_width,
#   height = plot_height
# )

# output$download_data <- downloadHandler(
#   filename = function() {"clonal_abundance.csv"},
#   content = function(file) {
#     write_csv(clonalAbundance()@abundance, file)
#   }
# )

# output$download_plot <- downloadHandler(
#   filename = function() {"clonal_abundance.pdf"},
#   content = function(file) {
#     ggsave(file, plot = clonalAbundancePlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
#   }
# )
