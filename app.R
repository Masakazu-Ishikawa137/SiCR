library(shiny)
library(Seurat)
library(tidyverse)
library(hrbrthemes)


source(paste0("Functions/230217script_first_functions_GEX.r"))
source(paste0("Functions/230217script_first_functions_TCR.r"))
source(paste0("Functions/230217script_first_functions_BCR.r"))
source(paste0('Functions/230217script_first_Run.r'))
source(paste0("Functions/230217script_second_functions_GEX.r"))
source(paste0("Functions/230217script_second_functions_TCR.r"))
source(paste0("Functions/230217script_second_functions_BCR.r"))


options(shiny.maxRequestSize=50*1024^2*1000)
options(shiny.port = 8100)


ui <- fluidPage(

  #Title####
  titlePanel("SCiR: Web Application for Single Cell Repertoire Analysis"),

  #Navbar####
  navbarPage("SiCR",

    #TabPanel, introduction####
    tabPanel("Introduction", mainPanel(
      HTML(
        '<h1>Welcome to SiCR!</h1>'
      )
    )),

    #TabPanel, upload####
    tabPanel("Upload",
      tabsetPanel(
        tabPanel("Cellranger output",
          sidebarLayout(
            sidebarPanel(
              HTML(
                '<h3>Upload cellranger output files</h3>
                <p>Upload these files and press "Run". Clustering will be started.</p>
                <h4>1. count file</h4>
                <p>This is the file for clustering.
                <p>.../outs/count/filtered_feature_bc_matrix.h5</p>
                <h4>2. TCR file(if exists)</h4>
                <p>This is the file for TCR. If you analyzed TCR, upload the csv file
                <p>.../outs/vdj_t/filtered_contig_annotations.csv</p>
                <h4>3. BCR file(if exists)</h4>
                <p>This is the file for BCR. If you analyzed BCR, upload the csv file
                <p>.../outs/vdj_b/filtered_contig_annotations.csv</p>'
              )
            ),
            mainPanel(
              fileInput("h5", "Choose .h5 file"),
              fileInput("tcr", "Choose .tcr file"),
              fileInput("bcr", "Choose .bcr file"),
              actionButton("Run", "Run"),
              textOutput("done_or_upload"),
            ),
          ),
        ),
        tabPanel("RDS file",
          sidebarLayout(
            sidebarPanel(
            ),
            mainPanel(
            ),
          ),
        ),
      ),
    ),

    #TabPanel, Basics
    tabPanel("Basics",
      sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
          downloadButton('downloadmetadata', 'Download Metadata')
        ),
      ),
    ),

    #TabPanel, gene expression####
    tabPanel("Gene Expression",
      sidebarLayout(
        sidebarPanel(
          selectInput("group_by", "group by?(default:seurat_clusters)", c("seurat_clusters")),
          sliderInput("point_size", "Size of points", min = 0.01, max = 10,value = 0.1, step = 0.01),
          sliderInput("label_size", "Size of labels", min = 0, max = 20, value = 10, step = 1),
          radioButtons("legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
          sliderInput("width_of_dimplot", "Width", min = 100, max = 2000, value = 500, step = 100),
          sliderInput("Height_of_dimplot", "Height", min = 100, max = 2000, value = 500, step = 100),
        ),
        mainPanel(
          plotOutput("clustering_plot")
        ),
      ),
    ),

    #TabPanel, TCR####
    tabPanel("TCR",
      tabsetPanel(

        tabPanel("Alpha Diversity",
          sidebarLayout(
            sidebarPanel(
              selectInput("TCR_alphadiversity_group", "Group", choices = list("Sample" = "Sample")),
              radioButtons("TCR_alphadiversity_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("TCR_width_of_alphadiversity", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("TCR_height_of_alphadiversity", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('TCR_alpha_diversity')
            )
          ),
        ),

        tabPanel("Clonotype expand",
          sidebarLayout(
            sidebarPanel(
              radioButtons("tcrexpand_heat_bar", "Heatmap or barplot?", choices = list("heatmap" = "heatmap", "barplot" = "barplot"), selected = "barplot"),
              selectInput("tcrexpand_group", "Group", choices = list("Sample" = "Sample")),
              selectInput("tcrexpand_sample", "Sample", choices = list("1" = "1", '2' = '2')),
              radioButtons("tcrexpand_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("width_of_clonotypeexpand", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("height_of_clonotypeexpand", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('tcr_expand')
            ),
          ),
        ),
        
        tabPanel("Gene usage",
          sidebarLayout(
            sidebarPanel(
              selectInput("tcrbarplot_x", "Gene", choices = list(
                  'TRA_v' = 'TCR_TRA_v_gene',
                  'TRA_j' = 'TCR_TRA_j_gene',
                  'TRB_v' = 'TCR_TRB_v_gene',
                  'TRB_d' = 'TCR_TRB_d_gene',
                  'TRB_j' = 'TCR_TRB_j_gene',
                  'TRB_c' = 'TCR_TRB_c_gene'
              )),
              selectInput("tcrbarplot_fill", "Group", choices = list("Sample" = "Sample")),
              radioButtons("tcrbarplot_verhori", "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
              radioButtons("tcrbarplot_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("TCR_width_of_barplot", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("TCR_height_of_barplot", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('tcr_barplot')
            ),
          ),
        ),
        
        tabPanel("Antigen prediction",
          sidebarLayout(
            sidebarPanel(
              selectInput("TCRclonotype", "Clonotype", choices = list("clonotype1" = "clonotype1")),
            ),
            mainPanel(
              tableOutput('TCR_antigen')
            ),
          ),
        ),
      )
    ),

    #TabPanel, BCR####
    tabPanel("BCR",
      tabsetPanel(

        tabPanel("Alpha Diversity",
          sidebarLayout(
            sidebarPanel(
              selectInput("BCR_alphadiversity_group", "Group", choices = list("Sample" = "Sample")),
              radioButtons("BCR_alphadiversity_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("BCR_width_of_alphadiversity", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("BCR_height_of_alphadiversity", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('BCR_alpha_diversity')
            ),
          )
        ),

        tabPanel("Clonotype expand",
          sidebarLayout(
            sidebarPanel(
              radioButtons("BCR_expand_heat_bar", "Heatmap or barplot?", choices = list("heatmap" = "heatmap", "barplot" = "barplot"), selected = "barplot"),
              selectInput("BCR_expand_group", "Group", choices = list("Sample" = "Sample")),
              selectInput("BCR_expand_sample", "Sample", choices = list("1" = "1", '2' = '2')),
              radioButtons("BCR_expand_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("BCR_width_of_clonotypeexpand", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("BCR_height_of_clonotypeexpand", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('BCR_expand')
            ),
          ),
        ),

        tabPanel("Gene usage",
          sidebarLayout(
            sidebarPanel(
              selectInput("BCR_barplot_x", "Gene", choices = list(
                'IGH_v' = 'BCR_IGH_v_gene',
                'IGL/K_v' = 'BCR_IGL_v_gene',
                'IGH_d' = 'BCR_IGH_d_gene',
                'IGH_j' = 'BCR_IGH_j_gene',
                'IGL_j' = 'BCR_IGL_j_gene',
                'IGH_c' = 'BCR_IGH_c_gene',
                'IGH_c' = 'BCR_IGL_c_gene'
              )),
              selectInput("BCR_barplot_fill", "Group", choices = list("Sample" = "Sample")),
              radioButtons("BCR_barplot_verhori", "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
              radioButtons("BCR_barplot_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("BCR_width_of_barplot", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("BCR_Height_of_barplot", "Height", min = 100, max = 2000, value = 500, step = 100),
            ),
            mainPanel(
              plotOutput('BCR_barplot')
            ),
          ),
        ),
        
        tabPanel("Antigen prediction",
          sidebarLayout(
            sidebarPanel(
              selectInput("BCR_clonotype", "Clonotype", choices = list("clonotype1" = "clonotype1")),
            ),
            mainPanel(
              tableOutput('BCR_antigen')
            ),
          ),
        ),
        
        tabPanel("Phylogenetic tree",
          sidebarLayout(
            sidebarPanel(
              textInput("bcr_clonotype", "clonotype?", value = "clonotype1"),
            ),
            mainPanel(
              plotOutput('bcr_phylogenetic_tree')
            ),
          ),
        ),

      )
    ),
    tabPanel("Subsetting",
      sidebarLayout(
        sidebarPanel(
          HTML(
            '<h3>Upload cellranger csv files</h3>
            <p>Upload these files and press "Run". Clustering will be started.</p>'
          ),
        ),
        mainPanel(
          fileInput("barcodes", "Choose .csv file"),
          actionButton("Run_subset", "Run"),
        ),
      ),
    ),
  )
)

########################################################################################################################

server <- function(input, output, session) {


  First_analysis <-function(){
    h5 <- input$h5$datapath
    tcr <- input$tcr$datapath
    bcr <- input$bcr$datapath
    if (is.null(h5)){
      done <- print('Pleaes upload .h5 file')
      return(done)
    }else{
      GEX(h5) -> seurat_object
      if(!is.null(tcr)){
        TCR(seurat_object, tcr) -> seurat_object
      }
      if(!is.null(bcr)){
        BCR(seurat_object, bcr) -> seurat_object
      }
      done <- print('Analysis was done!')
      return(list(done, seurat_object))
    }
  }

  add_clustering_plot <- function(seurat_object){
    umap_group <- mysinglecell_metadata_for_UMAP(seurat_object)
    updateSelectInput(session, "group_by", choices = umap_group)
    output$clustering_plot <- renderPlot(DimPlot(
      seurat_object,
      label = TRUE,
      pt.size = input$point_size,
      group.by =input$group_by,
      label.size = input$label_size) + theme(legend.position=input$legend),
      width = reactive(input$width_of_dimplot),
      height = reactive(input$Height_of_dimplot))
  }

  ####alpha diversity
  TCR_alpha_diversity <- function(seurat_object){
    metadata_group_for_TCR <- mysinglecell_metadata_for_TCR(seurat_object)
    updateSelectInput(session, "TCR_alphadiversity_group", choices = metadata_group_for_TCR)
    output$TCR_alpha_diversity <- renderPlot(alphaDiversity_TCR(seurat_object, group = input$TCR_alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$TCR_alphadiversity_legend), width = reactive(input$TCR_width_of_alphadiversity), height = reactive(input$TCR_height_of_alphadiversity))
  }

  ####Clonotype expand
  TCR_clonotype_expand <- function(seurat_object){
    metadata_group_for_TCR <- mysinglecell_metadata_for_TCR(seurat_object)
    updateSelectInput(session, "tcrexpand_group", choices = metadata_group_for_TCR)
    observe({
      unique_choice <- unique(seurat_object@meta.data[input$tcrexpand_group])
      updateSelectInput(session, "tcrexpand_sample", choices = unique_choice)
    })
      output$tcr_expand <- renderPlot(clonotype_expand_TCR(seurat_object, group = input$tcrexpand_group, sample = input$tcrexpand_sample, heat_bar = input$tcrexpand_heat_bar) + theme(legend.position=input$tcrexpand_legend), width = reactive(input$width_of_clonotypeexpand), height = reactive(input$height_of_clonotypeexpand))
  }

  ####Gene Usage
  TCR_gene_usage <-function(seurat_object){
    metadata_group_for_TCR <- mysinglecell_metadata_for_TCR(seurat_object)
    updateSelectInput(session, "tcrbarplot_fill", choices = metadata_group_for_TCR)
    output$tcr_barplot <- renderPlot(barplot(seurat_object, input$tcrbarplot_x, input$tcrbarplot_fill, input$tcrbarplot_verhori) + theme(legend.position=input$BCR_barplot_legend), width = reactive(input$TCR_width_of_barplot), height = reactive(input$TCR_height_of_barplot))
  }

  ####TCR antigen prediction
  TCR_antigen_annotation <- function(seurat_object){
    seurat_object@meta.data %>% 
    drop_na(TCR_TRB_raw_clonotype_id) -> metadata2
    clonotype_list <- unique(metadata2$TCR_TRB_raw_clonotype_id)
    clonotype_list <- sort(clonotype_list)
    updateSelectInput(session, "TCRclonotype", choices = clonotype_list)
    output$TCR_antigen <- renderTable(antigen_annotation_TCR(seurat_object, input$TCRclonotype))
  }

  ####BCR alpha diversity
  BCR_alpha_diversity <- function(seurat_object){
    metadata_group_for_BCR <- mysinglecell_metadata_for_BCR(seurat_object)
    updateSelectInput(session, "BCR_alphadiversity_group", choices = metadata_group_for_BCR)
    output$BCR_alpha_diversity <- renderPlot(alphaDiversity_BCR(seurat_object, group = input$BCR_alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$BCR_alphadiversity_legend), width = reactive(input$BCR_width_of_alphadiversity), height = reactive(input$BCR_height_of_alphadiversity))
  }

  ####Clonotype expand
  BCR_clonotype_expand <- function(seurat_object){
    metadata_group_for_BCR <- mysinglecell_metadata_for_BCR(seurat_object)
    updateSelectInput(session, "BCR_expand_group", choices = metadata_group_for_BCR)
    observe({
      unique_choice <- unique(seurat_object@meta.data[input$BCR_expand_group])
      updateSelectInput(session, "BCR_expand_sample", choices = unique_choice)
    })
      output$BCR_expand <- renderPlot(clonotype_expand_BCR(seurat_object, group = input$BCR_expand_group, sample = input$BCR_expand_sample, heat_bar = input$BCR_expand_heat_bar) + theme(legend.position=input$BCR_expand_legend), width = reactive(input$BCR_width_of_clonotypeexpand), height = reactive(input$BCR_height_of_clonotypeexpand))
  }

  ####Gene Usage
  BCR_gene_usage <-function(seurat_object){
    metadata_group_for_BCR <- mysinglecell_metadata_for_BCR(seurat_object)
    updateSelectInput(session, "BCR_barplot_fill", choices = metadata_group_for_BCR)
    output$BCR_barplot <- renderPlot(barplot(seurat_object, input$BCR_barplot_x, input$BCR_barplot_fill, input$BCR_barplot_verhori) + theme(legend.position=input$BCR_barplot_legend), width = reactive(input$BCR_width_of_barplot), height = reactive(input$BCR_height_of_barplot))
  }


  #First analysis
  Run_output <- eventReactive(input$Run,{
    First_analysis()
  })
  output$done_or_upload <- renderText({Run_output()[[1]]})

  observe({
    #Second analysis
    seurat_object <- Run_output()[[2]]
    # GEX
    # Show UMAP
    add_clustering_plot(seurat_object)
    if(sum(str_count(names(seurat_object@meta.data), 'TCR')) !=0){
      TCR_alpha_diversity(seurat_object)
      TCR_clonotype_expand(seurat_object)
      TCR_gene_usage(seurat_object)
      TCR_antigen_annotation(seurat_object)
    }
    if(sum(str_count(names(seurat_object@meta.data), 'BCR')) !=0){
      BCR_alpha_diversity(seurat_object)
      BCR_clonotype_expand(seurat_object)
      BCR_gene_usage(seurat_object)
    }
  })
}

shinyApp(ui = ui, server = server)



  #   #Second analysis
  #   if(!is.null(seurat_object)){
  #     # GEX
  #     # Show UMAP
  #     add_clustring_plot(seurat_object)
  #     # download metadata
  #     download_metadata()
  #     # TCR
  #     if (sum(str_count(names(seurat_object@meta.data), 'TCR') != 0)){
  #       ###alpha diversity
  #       TCR_processing()
  #     }
  #   }

  #   observeEvent(input$Run_subset,{
  #     if(!is.null(seurat_object) && !is.null(input$barcodes)){
  #       csv <- input$barcodes$datapath
  #       mysinglecell_subsetting(seurat_object, csv) -> seurat_object
  #       print('OK')
  #     }
  #   })
  # })


    # add_clustring_plot <- function(seurat_object){
    #   umap_group <- mysinglecell_metadata_for_UMAP(seurat_object)
    #   updateSelectInput(session, "group_by", choices = umap_group)
    #   output$clustering_plot <- renderPlot(DimPlot(
    #     seurat_object,
    #     label = TRUE,
    #     pt.size = input$point_size,
    #     group.by =input$group_by,
    #     label.size = input$label_size) + theme(legend.position=input$legend),
    #     width = reactive(input$width_of_dimplot),
    #     height = reactive(input$Height_of_dimplot))
    # }

    #   # Download metadata
    # download_metadata <- function(){
    #   output$downloadmetadata = downloadHandler(
    #     filename = "metadata.csv",
    #     content = function(file) {
    #     #??????????????????????????????????????????
    #     write.csv(seurat_object@meta.data, file)## write.csv()???write.tabel()???writeDoc?????????
    #     }
    #   )
    # }

  #Download metadata
    # download_metadata <- function(){
    #   output$downloadmetadata = downloadHandler(
    #     filename = "metadata.csv",
    #     content = function(file) {
    #     #??????????????????????????????????????????
    #     write.csv(seurat_object@meta.data, file)## write.csv()???write.tabel()???writeDoc?????????
    #     }
    #   )
    # }
