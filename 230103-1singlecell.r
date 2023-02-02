library(alakazam)
library(immunarch)
library(Seurat)
library(tidyverse)
library(scRepertoire)
library(hrbrthemes)
library(ggsci)

############Basic run

GEX <- function(h5){
    seurat_object <- mysinglecell_h5_to_umap(h5)
    seurat_object <- mysinglecell_celltyping_normal_normalization2(seurat_object)
    return (seurat_object)
}

GEX_TCR <- function(h5, tcr){
    GEX(h5) -> seurat_object
    mysinglecell_add_metadata(seurat_object, tcr) -> seurat_object
    myTCR_dataframe(tcr) -> tcr_dataframe
    myconcat_seumeta_tcr(seurat_object, tcr_dataframe) -> seurat_object    
    return (seurat_object)
}


GEX_BCR <- function(h5, bcr){
    GEX(h5) -> seurat_object
    mysinglecell_add_metadata(seurat_object, bcr) -> seurat_object
    myBCR_dataframe(bcr) -> bcr_dataframe
    myconcat_seumeta_bcr(seurat_object, bcr_dataframe) -> seurat_object    
    return (seurat_object)
}

GEX_TCR_BCR <- function(h5, tcr, bcr){
    GEX(h5) -> seurat_object
    mysinglecell_add_metadata(seurat_object, tcr) -> seurat_object
    myTCR_dataframe(tcr) -> tcr_dataframe
    myconcat_seumeta_tcr(seurat_object, tcr_dataframe) -> seurat_object    
    myBCR_dataframe(bcr) -> bcr_dataframe
    myconcat_seumeta_bcr(seurat_object, bcr_dataframe) -> seurat_object    
    return (seurat_object)
}

#########GEX########################################################################################################################
mysinglecell_h5_to_umap <- function(h5){
    h5 <- Read10X_h5(h5)
    seurat_object <- CreateSeuratObject(h5)
    str_split(rownames(seurat_object@meta.data), "-", simplify=TRUE) %>% .[,2] -> seurat_object@meta.data$Sample
    seurat_object <- NormalizeData(seurat_object)
    seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
    all.genes <- rownames(seurat_object)
    seurat_object <- ScaleData(seurat_object, features = all.genes)
    seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object))
    seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
    seurat_object <- FindClusters(seurat_object, resolution = 0.5)
    seurat_object <- RunUMAP(seurat_object, dims = 1:10)
    return (seurat_object)
}


mysinglecell_celltyping_normal_normalization2 <- function(seurat_object, type="Immune system"){
    suppressWarnings({sctype_load_gslist(type)}) -> gslist
    sctype_typescore_normal_normalization2(seurat_object, gslist) -> es.max
    sctype_clresults(seurat_object, es.max) -> result
    sctype_merge(seurat_object, result) -> final
    return(final)}

sctype_load_gslist <- function(type="Immune system"){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")

    # get cell-type-specific gene sets from our in-built database (DB)
    gs_list = gene_sets_prepare("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_short.xlsx", type) # e.g. Immune system, Liver, Pancreas, Kidney, Eye, Brain
    return (gs_list)
    }

sctype_typescore_normal_normalization2 <- function(data, gslist){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    ex.max <- sctype_score(scRNAseqData=data@assays$RNA@scale.data, scaled=TRUE, gs = gslist$gs_positive, gs2 = gslist$gs_negative)
    return (ex.max)
}

sctype_clresults <- function(pbmc, es.max){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    cL_resutls = do.call("rbind", lapply(unique(pbmc@meta.data$seurat_clusters), function(cl){
        es.max.cl = sort(rowSums(es.max[ ,rownames(pbmc@meta.data[pbmc@meta.data$seurat_clusters==cl, ])]), decreasing = !0)
        head(data.frame(cluster = cl, type = names(es.max.cl), scores = es.max.cl, ncells = sum(pbmc@meta.data$seurat_clusters==cl)), 10)
    }))
        sctype_scores = cL_resutls %>% group_by(cluster) %>% top_n(n = 1, wt = scores)  

    # set low-confident (low ScType score) clusters to "unknown"
    sctype_scores$type[as.numeric(as.character(sctype_scores$scores)) < sctype_scores$ncells/4] = "Unknown"
    return (sctype_scores)
}

sctype_merge <- function(immune.combined.sct, sctype_scores){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    immune.combined.sct@meta.data$celltype = ""
    for(j in unique(sctype_scores$cluster)){
      cl_type = sctype_scores[sctype_scores$cluster==j,]; 
      immune.combined.sct@meta.data$celltype[immune.combined.sct@meta.data$seurat_clusters == j] = as.character(cl_type$type[1])
}
    return (immune.combined.sct)
        }

####################################################################################################################################
######TCR or BCR#############
mysinglecell_add_metadata <- function(seurat_object, csv){
csv <- read.csv(csv)
str_split(csv$"barcode", "-", simplify=TRUE) %>% .[,2] -> csv$"Sample"
csv %>% dplyr::select(-barcode,-is_cell,-contig_id,-high_confidence,-length,-chain,-v_gene,-d_gene,-j_gene,-c_gene,-full_length,-productive,-fwr1,-fwr1_nt,-cdr1,-cdr1_nt,-fwr2,-fwr2_nt,-cdr2,-cdr2_nt,-fwr3,-fwr3_nt,-cdr3,-cdr3_nt,-fwr4,-fwr4_nt,-reads,-umis,-raw_clonotype_id,-raw_consensus_id,-exact_subclonotype_id) %>% unique() %>% dplyr::select(Sample, everything()) -> metadata
rowname <- rownames(seurat_object@meta.data)
dplyr::left_join(seurat_object@meta.data, metadata, by=c("Sample")) -> metadata
rownames(metadata) <- rowname
seurat_object@meta.data <- metadata
return(seurat_object)
    }


metadata_table <- function(csv){
    csv <- read.csv(csv)
    str_split(csv$"barcode", pattern = "-", simplify=TRUE) %>% .[,2] -> csv$"Sample"
    csv %>% dplyr::select(-barcode,-is_cell,-contig_id,-high_confidence,-length,-chain,-v_gene,-d_gene,-j_gene,-c_gene,-full_length,-productive,-fwr1,-fwr1_nt,-cdr1,-cdr1_nt,-fwr2,-fwr2_nt,-cdr2,-cdr2_nt,-fwr3,-fwr3_nt,-cdr3,-cdr3_nt,-fwr4,-fwr4_nt,-reads,-umis,-raw_clonotype_id,-raw_consensus_id,-exact_subclonotype_id) -> csv
    csv %>% dplyr::select(Sample, everything()) -> csv
    distinct(csv, Sample, .keep_all=TRUE) -> csv
    return(csv)
}
load_immunarch<-function(csv){
    if (dir.exists("test_dir")){
    unlink("test_dir", recursive = TRUE)
    }
    dir.create("test_dir")
    csv <- read.csv(csv)
    str_split(csv$"barcode", pattern = "-", simplify=TRUE) %>% .[,2] -> csv$"Sample"
    metadata_table("B_filtered_contig_annotations.csv") -> metadata
    csv %>% group_split(Sample) %>% setNames(unique(csv$"Sample")) -> list
    sample_number <- unique(csv$"Sample")
    map2(list, sample_number, function(x, y){write.csv(x, paste0("test_dir/", y, ".csv"), row.names=F, quote=F)})
    write.table(metadata, "test_dir/metadata.txt", quote=F, row.names=F, sep='\t')
    repLoad("test_dir") -> loaded
    return(loaded)
}



#########TCR######################################################
myTCR_dataframe <- function(sample){
    sample <- read.csv(sample)
    combinedTCR <- combineTCR(sample, samples = "NA", ID = NA, cells = "T-AB", filterMulti = TRUE)
    combinedTCR <- combinedTCR$NA_NA
    str_split(combinedTCR$barcode, pattern="_", simplify=TRUE) %>% .[,3] -> combinedTCR$barcode
    combinedTCR$sample <- NULL
    combinedTCR$ID <- NULL
    names(combinedTCR) <- str_c("pair_", names(combinedTCR))
    sample %>% dplyr::filter(chain == "TRA") -> TRA
    names(TRA) <- str_c("TRA_", names(TRA))
    TRA %>% distinct(TRA_barcode,.keep_all=TRUE)　-> TRA
    sample %>% dplyr::filter(chain == "TRB") -> TRB
    names(TRB) <- str_c("TRB_", names(TRB))
    TRB %>% distinct(TRB_barcode,.keep_all=TRUE)　-> TRB
    dplyr::left_join(combinedTCR, TRA, by=c("pair_barcode" = "TRA_barcode")) -> data_TRA
    dplyr::left_join(data_TRA, TRB, by=c("pair_barcode" = "TRB_barcode")) -> all_data
    names(all_data) <- str_c("TCR_", names(all_data))
    return(all_data)
}

myconcat_seumeta_tcr <- function(seurat_object, tcr){
    tibble::rownames_to_column(seurat_object@meta.data, "barcode") -> metadata
    dplyr::left_join(metadata, tcr, by=c("barcode" = "TCR_pair_barcode")) -> metadata
    tibble::column_to_rownames(metadata, "barcode") -> seurat_object@meta.data
    return(seurat_object)
}

estimateAbundance_TCR<- function(seurat_object, group = "Sample"){
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(all_of(group), TCR_TRB_raw_clonotype_id)
    curve <- estimateAbundance(metadata, group=group, ci=0.95, nboot=100, clone="TCR_TRB_raw_clonotype_id")
    plotAbundanceCurve(curve)
}

alphaDiversity_TCR<- function(seurat_object, group = "Sample", q = 'all'){
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(all_of(group), TCR_TRB_raw_clonotype_id)
    diversity <- alphaDiversity(metadata, group=group, min_q=0, max_q=4, step_q=1, nboot=100, clone="TCR_TRB_raw_clonotype_id")
    if (q == 'all'){
    p1 <- plot(diversity)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
        }
    else if(q != 'all'){
    p1 <- plot(diversity, q)# + theme_classic() + scale_color_nejm()　+ scale_fill_nejm() + ggtitle(NULL)
    }
    return(p1)

}

# TCR_antigen_annotation <- function(seurat_object, clonotype){
#     #download vdjdb
#     vdjdb <- dbLoad("https://gitlab.com/immunomind/immunarch/raw/dev-0.5.0/private/vdjdb.slim.txt.gz", .chain = "TRB", .species = "HomoSapiens", "vdjdb")
#     distinct(vdjdb, cdr3,.keep_all=TRUE) -> vdjdb
#     seurat_object[[]] -> metadata
#     dplyr::left_join(metadata, vdjdb, by=c("TCR_TRB_cdr3" = "cdr3")) -> metadata
#     metadata <- metadata %>% 
#         dplyr::select(TCR_TRB_raw_clonotype_id, antigen.epitope, antigen.gene, antigen.species, reference.id) %>% 
#             dplyr::filter(TCR_TRB_raw_clonotype_id == clonotype) %>% 
#                 distinct()
#     return(metadata)
# }

TCR_antigen_annotation <- function(seurat_object, clonotype){
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


#########BCR######################################################
myBCR_dataframe <- function(sample){
    sample <- read.csv(sample)
    combinedBCR <- combineBCR(sample, samples = "NA", ID = "NA", removeMulti = TRUE)
    combinedBCR <- combinedBCR$NA_NA
    str_split(combinedBCR$barcode, pattern="_", simplify=TRUE) %>% .[,3] -> combinedBCR$barcode
    combinedBCR$sample <- NULL
    combinedBCR$ID <- NULL
    names(combinedBCR) <- str_c("pair_", names(combinedBCR))
    sample %>% dplyr::filter(chain == "IGH") -> IGH
    names(IGH) <- str_c("IGH_", names(IGH))
    IGH %>% distinct(IGH_barcode,.keep_all=TRUE)　-> IGH
    sample %>% dplyr::filter(chain != "IGH") -> IGL
    names(IGL) <- str_c("IGL_", names(IGL))
    IGL %>% distinct(IGL_barcode,.keep_all=TRUE)　-> IGL
    dplyr::left_join(combinedBCR, IGH, by=c("pair_barcode" = "IGH_barcode")) -> data_IGH
    dplyr::left_join(data_IGH, IGL, by=c("pair_barcode" = "IGL_barcode")) -> all_data
    names(all_data) <- str_c("BCR_", names(all_data))
    return(all_data)
}

myconcat_seumeta_bcr <- function(seurat_object, bcr){
    tibble::rownames_to_column(seurat_object@meta.data, "barcode") -> metadata
    dplyr::left_join(metadata, bcr, by=c("barcode" = "BCR_pair_barcode")) -> metadata
    tibble::column_to_rownames(metadata, "barcode") -> seurat_object@meta.data
    return(seurat_object)
}

#ANALYSIS####################################################################################
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




clonotype_expand <- function(seurat_object, x = "TCR_TRB_raw_clonotype_id", group = "Sample", sample = "1", heat_bar = "heatmap", color = "Greys", range_color = 50, label_size=5, type = "Percent")
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



#####BCR########################
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

clonotype_expand_BCR <- function(seurat_object, x = "BCR_IGH_raw_clonotype_id", group = "Sample", sample = "1", heat_bar = "heatmap", color = "Greys", range_color = 50, label_size=5, type = "Percent")
{
    metadata <- seurat_object@meta.data
    # drop NA row in x or fill
    metadata <- metadata %>% drop_na(x, group)
    # group by fill and count all row in each group
    metadata <- group_by(metadata, metadata[group]) %>% add_count(name="n") %>% ungroup() 
    # calculate percentage of each clonotype in each group
    metadata <- group_by(metadata, metadata[x], metadata[group]) %>% add_count(name="Cell_number") %>% transform(Percent=Cell_number/n * 100)
    metadata <- distinct(metadata, metadata[x], metadata[group], n, Cell_number, Percent)
    dplyr::filter(metadata, metadata[group] == sample) %>% arrange(-Percent) %>% head(20) %>% .$"BCR_IGH_raw_clonotype_id" -> topclonotype
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

BCR_antigen_annotation <- function(seurat_object, clonotype){
    #download vdjdb
    csv <- read.csv('sceptre_result.csv')
    distinct(csv, chain1_cdr3_seq_calculated,.keep_all=TRUE) -> csv
    seurat_object[[]] -> metadata
    dplyr::left_join(metadata, csv, by=c("BCR_IGH_cdr3" = "chain1_cdr3_seq_calculated")) -> metadata
    metadata <- metadata %>% 
        dplyr::select(BCR_IGH_raw_clonotype_id, parent_antigen_name) %>% 
            dplyr::filter(BCR_IGH_raw_clonotype_id == clonotype) %>% 
                distinct()
    return(metadata)
}



phylogenetic_tree <- function(bcr, clone){
library(alakazam)
library(dowser)
ighv_reference <- read.csv("221124_ighv_reference.csv")
bcr <- read.csv(bcr)
bcr %>% dplyr::filter(chain == "IGH") -> bcr
bcr %>% dplyr::filter(raw_clonotype_id == clone) -> bcr
bcr %>% dplyr::filter(!is.na(v_gene)) -> bcr
as.data.frame(table(bcr$v_gene))[1,1] -> a
bcr$"germline_alignment" <- ighv_reference[a,]$Sequence
bcr$"germline_alignment_d_mask" <- ighv_reference[a,]$Sequence
bcr %>% mutate(length_cdr3_nt = nchar(cdr3_nt)) -> bcr
unite(bcr, "sequence_alignment", c('fwr1_nt','cdr1_nt','fwr2_nt','cdr2_nt','fwr3_nt','cdr3_nt','fwr4_nt'), sep="") -> bcr
bcr %>% dplyr::select(contig_id, sequence_alignment, germline_alignment, germline_alignment_d_mask, length_cdr3_nt, raw_clonotype_id, v_gene, j_gene) -> bcr
names(bcr) <- c("sequence_id", "sequence_alignment", "germline_alignment", "germline_alignment_d_mask", "junction_length", "clone_id", "v_call", "j_call")
clones = formatClones(bcr)
trees <- getTrees(clones)
plots <- plotTrees(trees)
plots[[1]]
}