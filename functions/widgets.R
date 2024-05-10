type_plot_clonotype <- function(ns){
    radioButtons(ns("type_plot_clonotype"), "Type of plot", choices = c("Rank abundance" = "rank_abundance", "Percent of unique clonotype" = "percent_of_unique_clonotype"), selected = "rank_abundance")
}

reduction_methodUI <- function(ns) {
    radioButtons(ns("reduction"), "Dimensional Reduction Method", choices = c("UMAP" = "umap", "T-SNE" = "tsne"), selected = "umap")
}

group_byUI <- function(ns) {
    radioButtons(ns("group_by"), "Group by", choices = c("seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}

split_byUI <- function(ns) {
    radioButtons(ns("split_by"), "Split by", choices = c("None" = "none", "seurat_clusters" = "seurat_clusters", "sample" = "sample"))
}

point_sizeUI <- function(ns) {
    sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10, value = 0.1, step = 0.01)
}

plot_size_and_download_pdfUI <- function(ns) {
    tagList(
        sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
        sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
        downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    )
}
plot_size_and_download_PDFUI <- function(ns) {
    tagList(
        sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
        sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
        downloadButton(ns("downloadPlot"), "Download Plot as PDF")
    )
}

chain_selectionUI <- function(ns, chain) {
    if (chain == "TCR") {
        radioButtons(ns("chain"), "Chain", choices = c("TRA" = "TRA", "TRB" = "TRB"), selected = "TRB")
    } else if (chain == "BCR") {
        radioButtons(ns("chain"), "Chain", choices = c("IGH" = "IGH", "IGL" = "IGL"), selected = "IGH")
    }
}

sequence_selectionUI <- function(ns) {
    radioButtons(ns("sequence"), "Sequence type", choices = c("Amino acid" = "aa", "Nucleotide" = "nt"), selected = "aa")
}

gene_selectionUI <- function(ns) {
    #    radioButtons(ns('gene'), "Gene", choices = c("Clonotype" = "raw_clonotype_id", "CDR3" = "cdr3", "V gene" = "v_gene", "D gene" = "d_gene", "J gene" = "j_gene", "C gene" = "c_gene"), selected = "raw_clonotype_id")
    #    radioButtons(ns('gene'), "Gene", choices = c("V gene" = "v_gene", "D gene" = "d_gene", "J gene" = "j_gene", "C gene" = "c_gene", "V-J gene" = 'v_j_gene'), selected = "v_gene")
    radioButtons(ns("gene"), "Gene", choices = c("V gene" = "v_gene", "D gene" = "d_gene", "J gene" = "j_gene", "C gene" = "c_gene"), selected = "v_gene")
}

clonotype_selectionUI <- function(ns) {
    #    radioButtons(ns('gene'), "Gene", choices = c("Clonotype" = "raw_clonotype_id", "CDR3" = "cdr3", "V gene" = "v_gene", "D gene" = "d_gene", "J gene" = "j_gene", "C gene" = "c_gene"), selected = "raw_clonotype_id")
    #    radioButtons(ns('gene'), "Gene", choices = c("V gene" = "v_gene", "D gene" = "d_gene", "J gene" = "j_gene", "C gene" = "c_gene", "V-J gene" = 'v_j_gene'), selected = "v_gene")
    radioButtons(ns("clonotype"), "Select", choices = c("Clonotype ID" = "raw_clonotype_id", "CDR3" = "cdr3"), selected = "raw_clonotype_id")
}

visibilityUI <- function(ns) {
    sliderInput(ns("visibility"), "Visibility", min = 0, max = 1, value = 1, step = 0.01)
}

xinterceptUI <- function(ns) {
    sliderInput(ns("xintercept"), "X intercept", min = -5, max = 5, value = c(-1, 1), step = 0.1)
}

yinterceptUI <- function(ns) {
    sliderInput(ns("yintercept"), "Y intercept", min = 0, max = 100, value = 1, step = 0.01)
}


label_sizeUI <- function(ns) {
    sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 5, step = 1)
}

legend_placeUI <- function(ns) {
    radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right")
}

plot_sizeUI <- function(ns) {
    tagList(
        sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
        sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100)
    )
}

gene_textUI <- function(ns) {
    textInput(ns("gene"), "Enter feature (gene) names (ex. CD3E, CD19, CD14):", value = "CD3E, CD19")
}

feature_columnUI <- function(ns) {
    numericInput(ns("feature_column"), label = "Number of columns", 1)
}

focus_groupUI <- function(ns) {
    checkboxGroupInput(ns("focus_group"), "Select groups to focus on", choices = "", inline = TRUE)
}

highlight_sizeUI <- function(ns) {
    sliderInput(ns("highlight_size"), "Size of highlight", min = 0.01, max = 10, value = 0.1, step = 0.01)
}

number_gene_labelUI <- function(ns) {
    numericInput(ns("number_gene_label"), "Number of gene labels", value = 10)
}

stackUI <- function(ns) {
    checkboxInput(ns("stack"), "Stack", value = FALSE)
}

flipUI <- function(ns) {
    checkboxInput(ns("flip"), "Flip", value = FALSE)
}

plot_typeUI <- function(ns) {
    radioButtons(ns("plot_type"), "type of plot", choices = c("Violin Plot" = "violin", "Dot Plot" = "dotplot", "Heatmap" = "heatmap"), selected = "violin")
}

ident1UI <- function(ns) {
    selectInput(ns("ident1"), "Group for comparison", choices = c("All group" = "all"))
}

ident2UI <- function(ns) {
    selectInput(ns("ident2"), "Against with", choices = c("Other groups " = "all"))
}

runbuttonUI <- function(ns) {
    actionButton(ns("run"), "Run")
}

download_csvUI <- function(ns) {
    downloadButton(ns("download"), "Download CSV")
}

scientific_colorUI <- function(ns) {
    radioButtons(ns("scientific_color"), "Scientific color", choices = list(
        "NPG" = "scale_fill_npg()",
        "AAAS" = "scale_fill_aaas()",
        "NEJM" = "scale_fill_nejm()",
        "Lancet" = "scale_fill_lancet()",
        "JAMA" = "scale_fill_jama()",
        "JCO" = "scale_color_jco() scale_fill_jco()",
        "UCSCGB" = "scale_fill_ucscgb()",
        "D3" = "scale_fill_d3()",
        "LocusZoom" = "scale_fill_locuszoom()",
        "IGV" = "scale_fill_igv()",
        "COSMIC" = "scale_fill_cosmic()",
        "UChicago" = "scale_fill_uchicago()",
        "Star Trek" = "scale_fill_startrek()",
        "Tron Legacy" = "scale_fill_tron()",
        "Futurama" = "scale_fill_futurama()",
        "Rick and Morty" = "scale_fill_rickandmorty()",
        "The Simpsons" = "scale_fill_simpsons()",
        "Flat UI" = "scale_fill_flatui()",
        "Frontiers" = "scale_fill_frontiers()",
        "GSEA" = "scale_fill_gsea()",
        "Material Design" = "scale_fill_material()"
    ))
}

methodUI <- function(ns) {
    radioButtons(ns("method"), "Calculate method", choices = c(
        "Public" = "public",
        "Jaccard" = "jaccard",
        "Tversky" = "tversky",
        "Morisita" = 'morisita'
    ))
}


clonotype_overlap_label <- function(ns){
    checkboxInput(ns("clonotype_overlap_label"), "Show label", value = TRUE)
}