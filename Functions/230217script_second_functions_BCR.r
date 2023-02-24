mysinglecell_metadata_for_BCR <- function(seurat_object){
    select(seurat_object@meta.data, !starts_with('TCR')) %>% select(!starts_with('BCR')) %>% select(-orig.ident, -nCount_RNA, -nFeature_RNA, -RNA_snn_res.0.5, -seurat_clusters) %>% names() -> group
    return(group)
}

alphaDiversity_BCR<- function(seurat_object, group = "Sample", q = 'all'){
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(all_of(group), BCR_IGH_raw_clonotype_id)
    diversity <- alphaDiversity(metadata, group=group, min_q=0, max_q=4, step_q=1, nboot=100, clone="BCR_IGH_raw_clonotype_id")
    if (q == 'all'){
    p1 <- plot(diversity)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
        }
    else if(q != 'all'){
    p1 <- plot(diversity, q)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
    }
    return(p1)

}

clonotype_expand_TCR <- function(seurat_object, x = "TCR_TRB_raw_clonotype_id", group = "Sample", sample = "1", heat_bar = "heatmap", color = "Greys", range_color = 50, label_size=5, type = "Percent")
{
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(x, group)
    # group by fill and count all row in each group
    metadata <- group_by(metadata, metadata[group]) %>% add_count(name="n") %>% ungroup() 
    # calculate percentage of each clonotype in each group
    metadata <- group_by(metadata, metadata[x], metadata[group]) %>% add_count(name="Cell_number") %>% transform(Percent=Cell_number/n * 100)
    metadata <- distinct(metadata, metadata[x], metadata[group], n, Cell_number, Percent)
    dplyr::filter(metadata, metadata[group] == sample) %>% arrange(-Percent) %>% head(20) %>% .$"TCR_TRB_raw_clonotype_id" -> topclonotype
     if(type == "Percent"){
         metadata <- distinct(metadata, metadata[x], metadata[group], n, Cell_number, Percent) %>% dplyr::select(-n, -Cell_number)
    }
     else if(type == "Cell_number"){
         metadata <- distinct(metadata, metadata[x], metadata[group], n, Cell_number, Percent) %>% dplyr::select(-n, -Percent)
     }
    #show non duplicated row
     metadata_pivot_wider <- metadata %>% pivot_wider(names_from = x, values_from = type)
     metadata_pivot_wider <- metadata_pivot_wider %>% dplyr::select(c(group, topclonotype))
     metadata_pivot_wider[is.na(metadata_pivot_wider)] <- 0
     metadata <- metadata_pivot_wider %>% pivot_longer(-group, values_to = type)
     if(heat_bar == "heatmap"){
#          ggplot(metadata, aes(x = reorder(name, -get(type)), get(group), fill = get(type))) + geom_tile(color = 'white', linewidth = 1) + geom_text(size=label_size, aes(label=round(get(type), 1))) + scale_fill_distiller(palette = color, direction = 1, limits = c(0,range_color)) + theme_ipsum() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90)) + guides(fill = guide_colourbar(title = type))
          ggplot(metadata, aes(x = reorder(name, -get(type)), get(group), fill = get(type))) + geom_tile() + geom_text(size=label_size, aes(label=round(get(type), 1))) + scale_fill_distiller(palette = color, direction = 1, limits = c(0,range_color)) + theme_ipsum() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90)) + guides(fill = guide_colourbar(title = type))

    }
     else if(heat_bar == "barplot"){
          ggplot(metadata, aes(x = reorder(name, -get(type)), y = get(type), fill = get(group))) + geom_bar(stat = "identity", position = "dodge")  + scale_y_continuous(expand = c(0,0)) + theme_classic() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 90, vjust=1)) + labs(fill = group) + scale_fill_nejm()
    }
}


barplot <- function(seurat_object, x, fill, ver_hor){
    metadata <- seurat_object@meta.data
    metadata %>% drop_na(x, fill) %>% select(x, fill) -> metadata
    metadata <- group_by(metadata, metadata[fill]) %>% add_count(name="n") %>% ungroup() 
    metadata <- group_by(metadata, metadata[x], metadata[fill]) %>% add_count(name="n1") %>% transform(Percent=n1/n * 100) 
    metadata <- distinct(metadata, metadata[x], metadata[fill], n, n1, Percent)
    if (ver_hor == "vertical") {
         ggplot(metadata, aes_string(x = x, y = "Percent", fill = fill)) + geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
            scale_y_continuous(expand = c(0, 0)) + theme_classic() + 
             theme(axis.text.x = element_text(angle = 90, vjust = 1)) + scale_fill_nejm()
     }
     else {
         ggplot(metadata, aes_string(x = x, y = "Percent", fill = fill)) + geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
             theme_classic() + coord_flip() + scale_y_continuous(expand = c(0, 0)) + scale_fill_nejm()
     }
}


antigen_annotation_TCR <- function(seurat_object, clonotype){
    #download vdjdb
    vdjdb <- dbLoad("https://gitlab.com/immunomind/immunarch/raw/dev-0.5.0/private/vdjdb.slim.txt.gz", .chain = "TRB", .species = "HomoSapiens", "vdjdb")
    distinct(vdjdb, cdr3,.keep_all=TRUE) -> vdjdb
    seurat_object[[]] -> metadata
    dplyr::left_join(metadata, vdjdb, by=c("TCR_TRB_cdr3" = "cdr3")) -> metadata
    metadata <- metadata %>% 
        dplyr::select(TCR_TRB_raw_clonotype_id, antigen.epitope, antigen.gene, antigen.species, reference.id) %>% 
            dplyr::filter(TCR_TRB_raw_clonotype_id == clonotype) %>% 
                distinct()
    return(metadata)
}
