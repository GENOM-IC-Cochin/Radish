# Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(colourpicker)
library(stringr)
library(ComplexHeatmap) #remplace pheatmap qui bug, mais la fonction reste
# library(pheatmap)
library(RColorBrewer)
library(tibble)
library(svglite)
library(ragg)

# Preliminary code
# update max upload size to 50mB
options(shiny.maxRequestSize = 50 * 1024^2)
themes_gg <- c("Classic",
               "Gray",
               "Classic with gridlines")
condition_colors <- brewer.pal(8, "Set2")