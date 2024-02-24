myseurat_normalize_umap_rerun <- function(seurat_object, low_nFeature, high_nFeature, low_Count, high_Count, mito){
  seurat_object <- subset(seurat_object, subset = nFeature_RNA > low_nFeature)
  seurat_object <- subset(seurat_object, subset = nFeature_RNA < high_nFeature)
  seurat_object <- subset(seurat_object, subset = nCount_RNA > low_Count)
  seurat_object <- subset(seurat_object, subset = nCount_RNA < high_Count)
  seurat_object <- subset(seurat_object, subset = percent.mt < mito)
  seurat_object <- myseurat_normalize_umap(seurat_object)
  return (seurat_object)
}

