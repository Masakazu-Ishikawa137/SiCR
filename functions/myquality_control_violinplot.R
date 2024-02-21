myquality_control_violinplot <- function(seurat_object, feature_low, feature_high, count_low, count_high, mito){
  vln1 <- VlnPlot(seurat_object, features = "nFeature_RNA", group.by='orig.ident') + theme(legend.position = "none") + geom_hline(yintercept=feature_low, col="red") + geom_hline(yintercept=feature_high, col="red")
  vln2 <- VlnPlot(seurat_object, features = "nCount_RNA", group.by='orig.ident') + theme(legend.position = "none") + geom_hline(yintercept=count_low, col="red") + geom_hline(yintercept=count_high, col="red")
  vln3 <- VlnPlot(seurat_object, features = "percent.mt", group.by='orig.ident') + theme(legend.position = "none") + geom_hline(yintercept=mito, col="red")
  plot <- ggarrange(vln1, vln2, vln3, ncol = 3)
  return(plot)
}

