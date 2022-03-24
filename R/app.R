# Central file

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

# Temporary source
# source("./module_gene_table.R")
# source("./module_pca.R")
# source("./module_maplot.R")
# source("./module_volcano.R")
# source("./module_heatmap.R")

# Preliminary code
# update max upload size to 1000mB
options(shiny.maxRequestSize = 1000 * 1024^2)
# All the objects expected in the input
expected_data <- c(
    "dataMerged",
    "all_results",
    "configuration",
    "contrasteList",
    "rld",
    "txi.rsem"
)
# The ggplot themes from which to choose
themes_gg <- c(
    "Classic",
    "Gray",
    "Classic with gridlines"
)
# Colors for the condition in the heatmap
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

ShareApp <- function() {
  global_theme <- create_theme( # one day in its own css
    theme = "flatly",
    adminlte_color(
      light_blue = "#006499"
    ),
    adminlte_global(
      content_bg = "#FFF",
      info_box_bg = "#EEF0F6"
    )
  )
  
# UI ---------------------------------------------------------------------------
  ui <- dashboardPage(
    header = dashboardHeader(
      title = "SHARE",
      leftUi = tagList(
        dropdownBlock(
          id = "contr",
          selectInput(
            inputId = "contrast_act",
            label = "Select the contrast you want to study",
            choices = NULL,
            selected = NULL
          ),
          title = "Current contrast :",
          badgeStatus = NULL
        ),
        # afficher le contraste
        textOutput("disp_contr"),
        # bricoler son apparence
        tags$head(tags$style("#disp_contr{
                                 margin-top: 7.5px;
                                 color: white;
                                 }")
        )
      )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Data", tabName = "inp"),
        menuItem("PCA", tabName = "pca"),
        menuItem("Interactive table", tabName = "tabl_gene"),
        menuItem("MA-plot", tabName = "maplot"),
        menuItem("Volcano plot", tabName = "volcano"),
        menuItem("Heatmap", tabName = "heatmap"),
        img(src = "./www/logo.svg",
            style="position:fixed;bottom:0;margin:0 0 15px 25px;",
            alt = "GENOM'IC")
      )
    ),
    body = dashboardBody(
      use_theme(global_theme),
      shinyFeedback::useShinyFeedback(),
      tabItems(
        tabItem(
          tabName = "home",
          tabBox(
            title = "SHARE : A Shiny app for RNA-Seq Exploration",
            width = 12,
            tabPanel(
              "About",
              includeMarkdown("www/intro.md")
            ),
            tabPanel(
              "Authors",
              includeMarkdown("www/authors.md")
            )
          )
        ),
        tabItem(
          tabName = "inp",
          InputUI("inp")
        ),
        tabItem(
          tabName = "pca",
          PcaUI("pca")
        ),
        tabItem(
          tabName = "tabl_gene",
          GeneTableUI("gntab")
        ),
        tabItem(
          tabName = "maplot",
          MAplotUI("ma")
        ),
        tabItem(
          tabName = "volcano",
          VolcanoUI("vp")
        ),
        tabItem(
          tabName = "heatmap",
          HeatmapUI("hm")
        )
      )
    )
  )
  
  
  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {
    my_values <- reactiveValues(
      given_genes_rows = NULL
    )
    observeEvent(list_loaded$all_results_names(), {
      updateSelectInput(
        inputId = "contrast_act",
        choices = list_loaded$all_results_names()
      )
    })
    
    output$disp_contr <- renderText({
      input$contrast_act
    })
    list_loaded <- InputServer("inp", reactive(input$contrast_act), input, output, session) 
    PcaServer(
      id = "pca",
      counts = list_loaded$counts,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      txi.rsem = list_loaded$txi.rsem,
      rld = list_loaded$rld,
      input = input,
      output = output,
      session = session
    )
    sel_info <- GeneTableServer(
      id = "gntab",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      contrast_act = reactive(input$contrast_act),
      input = input,
      output = output,
      session = session
    )
    MAplotServer(
      id = "ma",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      contrast_act = reactive(input$contrast_act),
      sel_genes_names = sel_info$sel_genes_names,
      sel_genes_ids = sel_info$sel_genes_ids,
      input = input,
      output = output,
      session = session
    )
    VolcanoServer(
      id = "vp",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      contrast_act = reactive(input$contrast_act),
      sel_genes_names = sel_info$sel_genes_names,
      sel_genes_ids = sel_info$sel_genes_ids,
      input = input,
      output = output,
      session = session
    )
    HeatmapServer(
      id = "hm",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive(input$contrast_act),
      sel_genes_names = sel_info$sel_genes_names,
      sel_genes_ids = sel_info$sel_genes_ids,
      input = input,
      output = output,
      session = session
    )
  }
  
  
  shinyApp(ui, server)
}