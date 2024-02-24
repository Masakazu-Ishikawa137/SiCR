get_available_genes <- function(seuratReactive) {
  return(eventReactive(seuratReactive, {
    if (!is.null(seuratReactive())) {
      rownames(seuratReactive()[["RNA"]])
    }
  }))
}