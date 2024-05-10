expression_distributionUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        plot_typeUI(ns),
        gene_textUI(ns),
        downloadButton(ns('available_feature'), "You can download available gene name"),
        group_byUI(ns),
        split_byUI(ns),
        feature_columnUI(ns),
        point_sizeUI(ns),
        stackUI(ns),
        flipUI(ns),
        plot_sizeUI(ns),
        downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    ),
    mainPanel(plotOutput(ns('plot')))
  )
}

expression_distributionServer <- function(id, myReactives) {
    moduleServer(id, function(input, output, session) {

        plot_width <- reactive(input$plot_width)
        plot_height <- reactive(input$plot_height)
        legend <- reactive(input$legend)

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols <- update_group_by3(myReactives)
        updateRadioButtons(session, "group_by", choices = group_cols)
      }
    })

    observeEvent(myReactives$seurat_object,{
      if(!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
        group_cols_split <- update_split_by_new(myReactives)
        group_cols_split[["None"]] <- "none"
        updateRadioButtons(session, "split_by", choices = group_cols_split, selected = "none")
      }
    })

        available_genes <- get_available_genes(reactive(myReactives$seurat_object))

        observe({
            req(myReactives$seurat_object) # ここでmyReactives$seurat_objectがNULLでないことを確認
            if (!is.null(input$gene)) {
                text_list <- unlist(strsplit(input$gene, ",\\s*"))
                # 利用可能な遺伝子のみをフィルタリング
                valid_genes <- text_list[text_list %in% available_genes()]

                if (length(valid_genes) > 0) {
                     if(input$plot_type == "violin"){
                         myReactives$expression_distribution_plot <- violinplot(myReactives, input, output, session, valid_genes)
                     }
                    else if(input$plot_type == "dotplot"){
                        myReactives$expression_distribution_plot <- dotplot(myReactives, input, output, session, valid_genes)
                    }
                    else if(input$plot_type == "heatmap"){
                        myReactives$expression_distribution_plot <- heatmap(myReactives, input, output, session, valid_genes)
                    }
                } else {
                    # 有効な遺伝子がない場合、ユーザーに通知
                    showNotification("指定された遺伝子がデータセットに存在しません。", type = "warning")
                }
            }
        })

        render_plot(output, 'plot', reactive({ myReactives$expression_distribution_plot }), plot_width, plot_height)
        setupDownloadPlotHandler(output, input, reactive({ myReactives$expression_distribution_plot }))
        download_available_genes(output, input, available_genes())

    })
}