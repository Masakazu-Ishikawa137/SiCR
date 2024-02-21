h5_to_seurat_object <- function(h5_path){
  h5 <- Read10X_h5(h5_path)
    if (!is.null(names(h5)) & ("Gene Expression" %in% names(h5))) { # names(h5) are NULL OR c('Gene Expression','Antibody Capture')
      h5 <- h5[["Gene Expression"]]
    }

  seurat_object <- CreateSeuratObject(h5)
  seurat_object@meta.data <- seurat_object@meta.data %>%
    mutate(barcode = rownames(.)) %>%
      mutate(sample = str_remove(barcode, "^.+-"))

  seurat_object <- NormalizeData(seurat_object)
  seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
  all.genes <- rownames(seurat_object)
  seurat_object <- ScaleData(seurat_object, features = all.genes)
  seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object), npcs = 50)
  seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
  seurat_object <- FindClusters(seurat_object, resolution = 0.5)
  seurat_object <- RunUMAP(seurat_object, dims = 1:10)
  seurat_object <- RunTSNE(seurat_object, dims = 1:10)
  return(seurat_object)
}