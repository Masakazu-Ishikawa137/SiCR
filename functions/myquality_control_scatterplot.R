myquality_control_scatterplot <- function(seurat_object, feature_low, feature_high, count_low, count_high, mito, plotsize){

plot1 <- seurat_object@meta.data %>% 
  ggplot(mapping = aes(x = nCount_RNA, y = percent.mt)) + 
  geom_pointdensity(size = plotsize) +
  scale_color_viridis(guide = 'none') +
  theme(legend.position = 'none') +
  theme_classic() +
  geom_vline(xintercept=count_low, col="red") +
  geom_vline(xintercept=count_high, col="red") +
  geom_hline(yintercept=mito, col="red")

plot1 <- ggMarginal(plot1, 
                    type = 'density', 
                    margins = 'both', 
                    fill = 'red')

plot2 <- seurat_object@meta.data %>% 
  ggplot(mapping = aes(x = nCount_RNA, y = nFeature_RNA)) + 
  geom_pointdensity(size = plotsize) +
  scale_color_viridis(guide = 'none') +
  theme(legend.position = 'none') +
  theme_classic() +
  geom_vline(xintercept=count_low, col="red") +
  geom_vline(xintercept=count_high, col="red") +
  geom_hline(yintercept=feature_low, col="red") +
  geom_hline(yintercept=feature_high, col="red")

plot2 <- ggMarginal(plot2, 
                    type = 'density', 
                    margins = 'both',
                    fill = 'red')


  plot <- ggarrange(plot1, plot2, ncol = 2)
  return(plot)
}