library("HGNChelper")
source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")

add_celltype <- function(seurat_object) {
  # Get list of cell-type-specific gene names
  # gs_list: list(gs_positive = list("celltype_A"=c("gene_a", "gene_b", ...), ...), gs_negative = list("celltype_A"=c("gene_i", "gene_j", ...), ...))
  gs_list = gene_sets_prepare("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_short.xlsx", "Immune system")
  # Get es.max (score matrix for each cell type in each cell (line, cell barcode)) based on seurat_object[["RNA"]]@scale.data, refering to gs_list
  # es.max: score matrix of celltype x cell barcode 
  es.max <- sctype_score(
    scRNAseqData = seurat_object[["RNA"]]@scale.data,
    scaled = TRUE,
    gs = gs_list$gs_positive,
    gs2 = gs_list$gs_negative
  )
  # Get top sctype_scores (cell type scores per seurat cluster. Sum scores per cluster and extract the top score celltype)
  sctype_scores <- map(unique(seurat_object@meta.data$seurat_clusters), function(cl){
    # es.max.cl.top: named vector（"cell type" = total_score）
    es.max.cl.top <- seurat_object@meta.data %>%
      filter(seurat_clusters == cl) %>%
      rownames() %>%
      es.max[,.] %>%
      rowSums() %>%
      sort(., decreasing = TRUE) %>%
      .[1] # extract only top score celltype for the cluster
    # Make dataframe (1 line)
    cL_resutls.cl <- data.frame(
      seurat_clusters = cl,
      celltype = names(es.max.cl.top),
      scores = es.max.cl.top,
      ncells = sum(seurat_object@meta.data$seurat_clusters == cl) # cell number for the cluster (constant value per cluster)
    )
    return(cL_resutls.cl)
  }) %>%
    reduce(rbind)
  # Set low-confident (low sctype score) clusters to "unknown"
  sctype_scores <- sctype_scores %>%
    mutate(celltype = if_else(scores < ncells/4, "Unknown", celltype))
  # Add celltype to seurat_object@meta.data
  seurat_object@meta.data <- seurat_object@meta.data %>%
    left_join(select(sctype_scores, seurat_clusters, celltype), by="seurat_clusters")
  rownames(seurat_object@meta.data) <- seurat_object@meta.data$barcode # Because left_join drop rownames (data.frame => tibble)
  return(seurat_object)
}