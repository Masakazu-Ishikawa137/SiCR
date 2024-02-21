library(ggsci)
library(RColorBrewer)
library(tidyverse)
library(Seurat)
library(shiny)
library(scRepertoire)

options(shiny.maxRequestSize=50*1024^2*1000)
options(shiny.port = 8100)

function_file_lst <- list.files("functions", pattern = ".R$", full.names = TRUE)
for (i in function_file_lst) {
  source(i)
}

module_file_lst <- list.files("module", pattern = ".R$", full.names = TRUE)
for (i in module_file_lst) {
  source(i)
}