# Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(colourpicker)
library(stringr)
# Preliminary code

# update max upload size to 50mB
options(shiny.maxRequestSize = 50 * 1024^2)
