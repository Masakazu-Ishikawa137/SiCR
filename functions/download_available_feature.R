download_available_genes <- function(output, input, available_genes) {
  output$available_feature <- downloadHandler(
    filename = function() {
      "available_genes.csv"
    },
    content = function(file) {
      write.csv(sort(available_genes), file, row.names = FALSE)
    }
  )
}