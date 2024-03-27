library(shiny)
library(sortable)
library(tidyverse)

ui <- fluidPage(
    uiOutput("dynamic_rank_list"),
    plotOutput('table') # UI側で動的なrank_listを出力するためのプレースホルダ
)

server <- function(input, output, session) {
  # ここで実際のデータまたはデモデータを用意

    data <- reactive({
        read.csv('tracking_df.csv') # 実際のデータパスに置き換えてください
    })

  samples <- reactive({ unique(data()[['sample']])}) # 実際にはデータに基づいてこの部分を更新
  
  output$dynamic_rank_list <- renderUI({
    rank_list(
      text = "サンプルをドラッグして順序を変更：",
      input_id = "ranked_samples",
      labels = samples() # サーバー側で定義したリアクティブなsamplesを使用
    )
  })

    output$table <- renderPlot({
          data() %>%
            mutate(sample = factor(sample, levels = input$ranked_samples)) %>%
            arrange(match(sample, input$ranked_samples)) %>% # ここで実際に行を並べ替える
            ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
        })

    }

shinyApp(ui, server)

# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(sortable)

# # UI定義
# ui <- fluidPage(
#     uiOutput('sortable'),
# # tableOutput("table")
# )

# # サーバー定義
# server <- function(input, output) {
#     # サンプルデータの読み込みをシミュレート
#     data <- reactive({
#         read.csv('tracking_df.csv') # 実際のデータパスに置き換えてください
#     })

#     samples <- reactive({ unique(data()[['sample']])}) # 実際にはデータに基づいてこの部分を更新


#    output$sortable <- renderUI({
#        rank_list(
#         text = "Drag column names to change order", 
#         labels = samples(),
#         input_id = "sortable")
#    })

#     # ボタンクリック時に実行されるリアクティブ式
#     observeEvent(input$submit_button, {
#         # リアクティブ値内でデータを更新
#         updated_data <- reactive({
#           data() %>%
#             mutate(sample = factor(sample, levels = input$ranked_samples)) %>%
#             arrange(match(sample, input$ranked_samples)) # ここで実際に行を並べ替える
#         })

#         # テーブル出力を更新
#         output$table <- renderTable({
#           updated_data() # 更新されたデータを表示
#         })
#     })
# }

# # アプリの実行
# shinyApp(ui = ui, server = server)