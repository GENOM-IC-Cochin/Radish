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
library(ComplexHeatmap) # remplace pheatmap qui bug, mais la fonction reste
library(DESeq2)
library(RColorBrewer)
library(svglite)
library(ragg)
library(markdown)

# Preliminary code
# update max upload size to 100mB
options(shiny.maxRequestSize = 1000 * 1024^2)
expected_data <- c(
    "dataMerged",
    "all_results",
    "configuration",
    "contrasteList",
    "rld",
    "txi.rsem"
)
themes_gg <- c(
    "Classic",
    "Gray",
    "Classic with gridlines"
)
condition_colors <- brewer.pal(8, "Set2")
# Columns always present in the table (to be renamed by DT)
base_table_columns <- c(
    "Row.names",
    "baseMean",
    "log2FoldChange",
    "padj",
    "symbol",
    "description"
)
