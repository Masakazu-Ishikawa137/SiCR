library(shiny)
library(Seurat)
library(tidyverse)
library(hrbrthemes)
source("230103-1singlecell.r")
source('230217singlecell_analysis.r')
options(shiny.maxRequestSize=50*1024^2*1000)
options(shiny.port = 8100)


ui <- fluidPage(

  #Title####
  titlePanel("SCiR"),

  #Navbar####
  navbarPage("Repertoverse",

    #TabPanel, introduction####
    tabPanel("Introduction", mainPanel()),

    #TabPanel, upload####
    tabPanel("Upload",
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
        textOutput("pleaseupload"),
      )
    ),
#TabPanel, upload####
               tabPanel("Subsetting",
                sidebarPanel(HTML(
                      '<h3>Upload cellranger csv files</h3>
                      <p>Upload these files and press "Run". Clustering will be started.</p>
                      ')),
                mainPanel(
                  fileInput("barcodes", "Choose .csv file"),
                  actionButton("Run_subset", "Run"),
                        )

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
          sidebarPanel(
            selectInput("alphadiversity_group", "Group", choices = list("Sample" = "Sample")),
            radioButtons("alphadiversity_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
            sliderInput("width_of_alphadiversity", "Width", min = 100, max = 2000, value = 500, step = 100),
            sliderInput("height_of_alphadiversity", "Height", min = 100, max = 2000, value = 500, step = 100),
          ),
          mainPanel(
            plotOutput('alpha_diversity')
          )
        ),

        tabPanel("Clonotype expand",
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
          )
        ),
        
        tabPanel("Gene usage",
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
            sliderInput("width_of_tcrbarplot", "Width", min = 100, max = 2000, value = 500, step = 100),
            sliderInput("Height_of_tcrbarplot", "Height", min = 100, max = 2000, value = 500, step = 100),
          ),
          mainPanel(
            plotOutput('tcr_barplot')
          )
        ),
        
        tabPanel("Antigen prediction",
          sidebarPanel(
            selectInput("TCRclonotype", "Clonotype", choices = list("clonotype1" = "clonotype1")),
          ),
          mainPanel(
            tableOutput('TCR_antigen')
          )
        ),
      )
    ),

    #TabPanel, BCR####
    tabPanel("BCR",
      tabsetPanel(

        tabPanel("Alpha Diversity",
          sidebarPanel(
            selectInput("BCR_alphadiversity_group", "Group", choices = list("Sample" = "Sample")),
              radioButtons("BCR_alphadiversity_legend", "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
              sliderInput("BCR_width_of_alphadiversity", "Width", min = 100, max = 2000, value = 500, step = 100),
              sliderInput("BCR_height_of_alphadiversity", "Height", min = 100, max = 2000, value = 500, step = 100),
          ),
          mainPanel(
            plotOutput('BCR_alpha_diversity')
          )
        ),

        tabPanel("Clonotype expand",
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
          )
        ),

        tabPanel("Gene usage",
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
          )
        ),
        
        tabPanel("Antigen prediction",
          sidebarPanel(
            selectInput("BCR_clonotype", "Clonotype", choices = list("clonotype1" = "clonotype1")),
          ),
          mainPanel(
            tableOutput('BCR_antigen')
          )
        ),
        
        tabPanel("Phylogenetic tree",
          sidebarPanel(
            textInput("bcr_clonotype", "clonotype?", value = "clonotype1"),
          ),
          mainPanel(
            plotOutput('bcr_phylogenetic_tree')
          )
        ),

      )
    ),

  )
)

########################################################################################################################

server <- function(input, output, session) {

  observeEvent(input$Run,{

    add_clustring_plot <- function(){
      output$clustering_plot <- renderPlot(DimPlot(
        seurat_object,
        label = TRUE,
        pt.size = input$point_size,
        group.by =input$group_by,
        label.size = input$label_size) + theme(legend.position=input$legend),
        width = reactive(input$width_of_dimplot),
        height = reactive(input$Height_of_dimplot))
    }

    TCR_processing <- function(){
      tcr_csv <- read.csv(tcr)
      ####alpha diversity
      updateSelectInput(session, "alphadiversity_group", choices = list)
      output$alpha_diversity <- renderPlot(alphaDiversity_TCR(seurat_object, group = input$alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$alphadiversity_legend), width = reactive(input$width_of_alphadiversity), height = reactive(input$height_of_alphadiversity))
      ####Clonotype expand
      updateSelectInput(session, "tcrexpand_group", choices = list)
      observe({
        unique_choice <- unique(seurat_object@meta.data[input$tcrexpand_group])
        updateSelectInput(session, "tcrexpand_sample", choices = unique_choice)
      })
      output$tcr_expand <- renderPlot(clonotype_expand(seurat_object, group = input$tcrexpand_group, sample = input$tcrexpand_sample, heat_bar = input$tcrexpand_heat_bar) + theme(legend.position=input$tcrexpand_legend), width = reactive(input$width_of_clonotypeexpand), height = reactive(input$height_of_clonotypeexpand))
      ####Gene Usage
      updateSelectInput(session, "tcrbarplot_fill", choices = list)
      output$tcr_barplot <- renderPlot(barplot(seurat_object, input$tcrbarplot_x, input$tcrbarplot_fill, input$tcrbarplot_verhori) + theme(legend.position=input$BCR_barplot_legend), width = reactive(input$BCR_width_of_barplot), height = reactive(input$BCR_Height_of_barplot))
      ####TCR antigen
      seurat_object@meta.data %>% 
        drop_na(TCR_TRB_raw_clonotype_id) -> metadata2
      clonotype_list <- unique(metadata2$TCR_TRB_raw_clonotype_id)
      clonotype_list <- sort(clonotype_list)
      updateSelectInput(session, "TCRclonotype", choices = clonotype_list)
      output$TCR_antigen <- renderTable(TCR_antigen_annotation(seurat_object, input$TCRclonotype))
    }

    BCR_processing <- function(){
      bcr_csv <- read.csv(bcr)
      ####alpha diversity
      updateSelectInput(session, "BCR_alphadiversity_group", choices = list)
      output$BCR_alpha_diversity <- renderPlot(alphaDiversity_BCR(seurat_object, group = input$BCR_alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$BCR_alphadiversity_legend), width = reactive(input$BCR_width_of_alphadiversity), height = reactive(input$BCR_height_of_alphadiversity))
      ####Clonotype expand
      updateSelectInput(session, "BCR_expand_group", choices = list)
      observe({
        unique_choice <- unique(seurat_object@meta.data[input$BCR_expand_group])
        updateSelectInput(session, "BCR_expand_sample", choices = unique_choice)
      })
      output$BCR_expand <- renderPlot(clonotype_expand_BCR(seurat_object, group = input$BCR_expand_group, sample = input$BCR_expand_sample, heat_bar = input$BCR_expand_heat_bar) + theme(legend.position=input$BCR_expand_legend), width = reactive(input$BCR_width_of_clonotypeexpand), height = reactive(input$BCR_height_of_clonotypeexpand))
      ####Gene Usage
      updateSelectInput(session, "BCR_barplot_fill", choices = list)
      output$BCR_barplot <- renderPlot(barplot(seurat_object, input$BCR_barplot_x, input$BCR_barplot_fill, input$BCR_barplot_verhori) + theme(legend.position=input$tcrbarplot_legend), width = reactive(input$width_of_tcrbarplot), height = reactive(input$Height_of_tcrbarplot))
      ####BCR antigen
      seurat_object@meta.data %>% 
        drop_na(BCR_IGH_raw_clonotype_id) -> metadata2
      clonotype_list <- unique(metadata2$BCR_IGH_raw_clonotype_id)
      clonotype_list <- sort(clonotype_list)
      updateSelectInput(session, "BCR_clonotype", choices = clonotype_list)
      output$BCR_antigen <- renderTable(BCR_antigen_annotation(seurat_object, input$BCR_clonotype))
      ###Phylogenetic tree
      output$bcr_phylogenetic_tree <- renderPlot(phylogenetic_tree(bcr, input$bcr_clonotype))
    }

    h5 <- input$h5$datapath
    tcr <- input$tcr$datapath
    bcr <- input$bcr$datapath

    if (is.null(h5)){
      output$pleaseupload <- renderText("Please upload .h5 file")
    }else{
      GEX(h5) -> seurat_object
      if(!is.null(tcr)){
        TCR(seurat_object, tcr) -> seurat_object
      }
      if(!is.null(bcr)){
        BCR(seurat_object, bcr) -> seurat_object
      }
    }

    # } else if (is.null(tcr) && is.null(bcr)) {
    #   cmbn <- "GEX"
    # } else if (is.null(bcr)){
    #   cmbn <- "GEX_TCR"
    # } else if (is.null(tcr)){
    #   cmbn <- "GEX_BCR"
    # } else {
    #   cmbn <- "GEX_TCR_BCR"
    # }

    # if (cmbn == "GEX"){
    #   seurat_object <- GEX(h5)
    #   add_clustring_plot()
    # }

    # if (cmbn == "GEX_TCR"){
    #   seurat_object <- GEX_TCR(h5, tcr)
    #   list <- names(metadata_table(tcr)) # 変数名が一般の関数名とかぶるのは良くないとされているので、listではなくてgroupsとかにした方がいいかも
    #   list2 <- c('seurat_clusters', 'celltype', list)
    #   updateSelectInput(session, "group_by", choices = list2)
    #   add_clustring_plot()
    #   TCR_processing()
    # }

    # if (cmbn == "GEX_BCR"){
    #   seurat_object <- GEX_BCR(h5, bcr)
    #   list <- names(metadata_table(bcr))
    #   add_clustring_plot()
    #   BCR_processing()
    # }

  })

}


shinyApp(ui = ui, server = server)


# ###############

# ## NO upload      
#       if(is.null(input$h5)){
#         output$pleaseupload <- renderText("Please upload .h5 file")
#       }



# ## GEX
#       else if(!is.null(input$h5) && is.null(input$tcr) && is.null(input$bcr)){
#           GEX(h5) -> seurat_object

# ### Clustering plot
#           output$clustering_plot <- renderPlot(DimPlot(seurat_object, label=TRUE, pt.size= input$point_size, group.by=input$group_by, label.size = input$label_size) + theme(legend.position=input$legend), width = reactive(input$width_of_dimplot), height = reactive(input$Height_of_dimplot))
#         }







# ## GEX TCR
#       else if(!is.null(input$h5) && !is.null(input$tcr) && is.null(input$bcr)){
#           GEX_TCR(h5, tcr) -> seurat_object
#           list <- names(metadata_table(tcr))


# ### Clustering plot
#           list2 <- c('seurat_clusters', 'celltype', list)
#           updateSelectInput(session, "group_by", choices = list2)
#           output$clustering_plot <- renderPlot(DimPlot(seurat_object, label=TRUE, pt.size= input$point_size, group.by=input$group_by, label.size = input$label_size) + theme(legend.position=input$legend), width = reactive(input$width_of_dimplot), height = reactive(input$Height_of_dimplot))

# ###TCR
#           tcr_csv <- read.csv(tcr)
# ####alpha diversity
#           updateSelectInput(session, "alphadiversity_group", choices = list)
#           output$alpha_diversity <- renderPlot(alphaDiversity_TCR(seurat_object, group = input$alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$alphadiversity_legend), width = reactive(input$width_of_alphadiversity), height = reactive(input$height_of_alphadiversity))
# ####Clonotype expand
#           updateSelectInput(session, "tcrexpand_group", choices = list)
#           observe({
#               unique_choice <- unique(seurat_object@meta.data[input$tcrexpand_group])
#               updateSelectInput(session, "tcrexpand_sample", choices = unique_choice)
#                   })
#           output$tcr_expand <- renderPlot(clonotype_expand(seurat_object, group = input$tcrexpand_group, sample = input$tcrexpand_sample, heat_bar = input$tcrexpand_heat_bar) + theme(legend.position=input$tcrexpand_legend), width = reactive(input$width_of_clonotypeexpand), height = reactive(input$height_of_clonotypeexpand))

# ####Gene Usage
#           updateSelectInput(session, "tcrbarplot_fill", choices = list)
#           output$tcr_barplot <- renderPlot(barplot(seurat_object, input$tcrbarplot_x, input$tcrbarplot_fill, input$tcrbarplot_verhori) + theme(legend.position=input$BCR_barplot_legend), width = reactive(input$BCR_width_of_barplot), height = reactive(input$BCR_Height_of_barplot))

# ####TCR antigen
#         seurat_object@meta.data %>% 
#           drop_na(TCR_TRB_raw_clonotype_id) -> metadata2
#         clonotype_list <- unique(metadata2$TCR_TRB_raw_clonotype_id)
#         clonotype_list <- sort(clonotype_list)
#         updateSelectInput(session, "TCRclonotype", choices = clonotype_list)
#         output$TCR_antigen <- renderTable(TCR_antigen_annotation(seurat_object, input$TCRclonotype))

#         }

# ## GEX BCR
#       else if(!is.null(input$h5) && is.null(input$tcr) && !is.null(input$bcr)){
#           GEX_BCR(h5, bcr) -> seurat_object
#           list <- names(metadata_table(bcr))
# ### Clustering plot
#           output$clustering_plot <- renderPlot(DimPlot(seurat_object, label=TRUE, pt.size= input$point_size, group.by=input$group_by, label.size = input$label_size) + theme(legend.position=input$legend), width = reactive(input$width_of_dimplot), height = reactive(input$Height_of_dimplot))

# ##BCR
#           bcr_csv <- read.csv(bcr)
# ####alpha diversity
#           updateSelectInput(session, "BCR_alphadiversity_group", choices = list)
#           output$BCR_alpha_diversity <- renderPlot(alphaDiversity_BCR(seurat_object, group = input$BCR_alphadiversity_group) + theme_classic() + scale_fill_nejm() + ggtitle(NULL) + scale_color_nejm() + theme(legend.position=input$BCR_alphadiversity_legend), width = reactive(input$BCR_width_of_alphadiversity), height = reactive(input$BCR_height_of_alphadiversity))

# ####Clonotype expand
#           updateSelectInput(session, "BCR_expand_group", choices = list)
#           observe({
#               unique_choice <- unique(seurat_object@meta.data[input$BCR_expand_group])
#               updateSelectInput(session, "BCR_expand_sample", choices = unique_choice)
#                   })
#           output$BCR_expand <- renderPlot(clonotype_expand_BCR(seurat_object, group = input$BCR_expand_group, sample = input$BCR_expand_sample, heat_bar = input$BCR_expand_heat_bar) + theme(legend.position=input$BCR_expand_legend), width = reactive(input$BCR_width_of_clonotypeexpand), height = reactive(input$BCR_height_of_clonotypeexpand))

# ####Gene Usage
#           updateSelectInput(session, "BCR_barplot_fill", choices = list)
#           output$BCR_barplot <- renderPlot(barplot(seurat_object, input$BCR_barplot_x, input$BCR_barplot_fill, input$BCR_barplot_verhori) + theme(legend.position=input$tcrbarplot_legend), width = reactive(input$width_of_tcrbarplot), height = reactive(input$Height_of_tcrbarplot))

# ####BCR antigen
#         seurat_object@meta.data %>% 
#           drop_na(BCR_IGH_raw_clonotype_id) -> metadata2
#         clonotype_list <- unique(metadata2$BCR_IGH_raw_clonotype_id)
#         clonotype_list <- sort(clonotype_list)
#         updateSelectInput(session, "BCR_clonotype", choices = clonotype_list)
#         output$BCR_antigen <- renderTable(BCR_antigen_annotation(seurat_object, input$BCR_clonotype))


# ###Phylogenetic tree
#           output$bcr_phylogenetic_tree <- renderPlot(phylogenetic_tree(bcr, input$bcr_clonotype))
#         }




# ## GEX TCR BCR
#       else if(!is.null(input$h5) && !is.null(input$tcr) && !is.null(input$bcr)){
#           GEX_TCR_BCR(h5, tcr, bcr) -> seurat_object

# ### Clustering plot
#           output$clustering_plot <- renderPlot(DimPlot(seurat_object, label=TRUE, pt.size= input$point_size, group.by=input$group_by, label.size = input$label_size) + theme(legend.position=input$legend), width = reactive(input$width_of_dimplot), height = reactive(input$Height_of_dimplot))


#         }



#         })}



