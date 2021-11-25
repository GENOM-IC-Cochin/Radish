# Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(fresh)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(colourpicker)
library(stringr)
library(ComplexHeatmap) #remplace pheatmap qui bug, mais la fonction reste
library(DESeq2)
library(RColorBrewer)
library(svglite)
library(ragg)
library(markdown)

# Preliminary code
# update max upload size to 50mB
options(shiny.maxRequestSize = 100 * 1024^2)
themes_gg <- c("Classic",
               "Gray",
               "Classic with gridlines")
condition_colors <- brewer.pal(8, "Set2")
# Columns always present in the table (to be renamed)
base_table_columns <- c("baseMean",
                        "log2FoldChange",
                        "padj",
                        "ensembl_gene_id",
                        "symbol")
