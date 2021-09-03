# Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DESeq2)

# Preliminary code

# update max upload size to 50mB
options(shiny.maxRequestSize = 50 * 1024^2)
