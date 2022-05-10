# Central file

# Libraries
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinyvalidate)
library(DT)
library(fresh)
library(plotly)
library(magrittr)
library(waiter)
library(purrr)
library(dplyr)
library(tibble)
library(scales)
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
# update max upload size to 1000mB
options(shiny.maxRequestSize = 1000 * 1024^2,
        shiny.useragg = TRUE)

# The ggplot themes from which to choose
themes_gg <- c(
    "Classic",
    "Gray",
    "Classic with gridlines"
)

# Columns always present in the table (to be renamed by DT)
base_table_columns <- c(
    "Row.names",
    "baseMean",
    "log2FoldChange",
    "padj",
    "symbol",
    "description"
)

video_width <- 320

ShareApp <- function(...) {
  global_theme <- create_theme(
    bs4dash_status(
      primary = "#006499",
      secondary = "#009982"
    ),
    bs4dash_sidebar_light(
      bg = "#404040",
      color = "#FFF"
    )
  )
  # Allows use the www directory (need to use www in path though)
  addResourcePath(prefix = "www", directoryPath = "./www")  
# UI ---------------------------------------------------------------------------
  ui <- dashboardPage(
    freshTheme = global_theme,
    header = dashboardHeader(
      skin = "light",
      status = "primary",
      title = dashboardBrand(
        title = "SHARE",
        color = "primary"
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Tutorial", tabName = "tut"),
        menuItem("Input data", tabName = "inp"),
        menuItem("PCA", tabName = "pca"),
        menuItem(p("Table", style = "font-weight : bold; text-decoration: underline;"),
                 tabName = "tabl_gene"),
        menuItem("MA-plot", tabName = "maplot"),
        menuItem("Volcano plot", tabName = "volcano"),
        menuItem("Heatmap", tabName = "heatmap")
      ),
      hr(),
      selectInput(
            inputId = "contrast_act",
            label = div("Select the contrast", style = "color : white"),
            choices = NULL,
            selected = NULL
          ),
      hr(),
      tags$a(tags$img(src = "www/logo.svg",
               style="position:fixed;bottom:0;margin:0 0 15px 25px;",
               alt = "GENOM'IC"),
             href = "https://institutcochin.fr/genomic",
             target = "_blank"
      )
    ),
    body = dashboardBody(
      shinyFeedback::useShinyFeedback(),
      useWaiter(),
      tabItems(
        tabItem(
          tabName = "home",
          tabBox(
            title = div("SHARE : A Shiny app for RNA-Seq Exploration",
                        style = "font-weight : bold"),
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
          tabName = "tut",
          tabBox(
              width = 12,
            tabPanel(
              "PCA",
              includeHTML("www/tutorials/pca.html")
            ),
            tabPanel(
              "Table",
              includeHTML("www/tutorials/table.html")
            ),
            tabPanel(
              "MA-plot",
              withMathJax(includeHTML("www/tutorials/maplot.html"))
            ),
            tabPanel(
              "Volcano Plot",
              includeHTML("www/tutorials/volcano.html")
            ),
            tabPanel(
              "Heatmap",
              includeHTML("www/tutorials/heatmap.html")
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
    observeEvent(list_loaded$all_results_choice(), {
      updateSelectInput(
        inputId = "contrast_act",
        choices = list_loaded$all_results_choice()
      )
    })


    list_loaded <- InputServer("inp", reactive(input$contrast_act))

    PcaServer(
      id = "pca",
      counts = list_loaded$counts,
      config = list_loaded$config,
      txi.rsem = list_loaded$txi.rsem,
      rld = list_loaded$rld
    )
    sel_table <- GeneTableServer(
      id = "gntab",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive(input$contrast_act) #should be recalculated with change
    )
    MAplotServer(
      id = "ma",
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive(input$contrast_act),
      contrastes = list_loaded$contrastes,
      sel_genes_table = sel_table
    )
    VolcanoServer(
      id = "vp",
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive(input$contrast_act),
      contrastes = list_loaded$contrastes,
      sel_genes_table = sel_table
    )
    HeatmapServer(
      id = "hm",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive(input$contrast_act),
      contrastes = list_loaded$contrastes,
      sel_genes_table = sel_table
    )
  }
  
  
  shinyApp(ui,
           server,
           ...)
}
