#  download .rds section was commented out.

uploadCellranger_sidebarUI <- function(id) {
    ns <- NS(id)
    sidebarPanel(
    h3("Upload cellranger output files"),
    p('Upload these files and press "Run".'),
    h4("1. count file"),
    p("This is the file for clustering."),
    p(".../outs/count/filtered_feature_bc_matrix.h5"),
    h4("2. TCR file (if exists)"),
    p("This is the file for TCR. If you analyzed TCR, upload the csv file"),
    p(".../outs/vdj_t/filtered_contig_annotations.csv"),
    h4("3. BCR file (if exists)"),
    p("This is the file for BCR. If you analyzed BCR, upload the csv file"),
    p(".../outs/vdj_b/filtered_contig_annotations.csv"),
  )
}



uploadCellrangerServer <- function(id) {
  moduleServer(id, function(input, output, session) {}
)}