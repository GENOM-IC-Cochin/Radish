# Preliminary code
# update max upload size to 1000mB
options(
  shiny.maxRequestSize = 1000 * 1024^2,
  shiny.useragg = TRUE
)

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

global_theme <- fresh::create_theme(
  fresh::bs4dash_status(
    primary = "#006499",
    secondary = "#91730F",
    info = "#07856E"
  ),
  fresh::bs4dash_sidebar_light(
    bg = "#404040",
    color = "#FFF"
  )
)


##' The UI of the Radish application
##'
##' @title Radish UI
##' @author Paul
##' @export
radish_ui <- function() {
  ui <- dashboardPage(
    freshTheme = global_theme,
    header = dashboardHeader(
      title = tags$img(src = "logo_app_3.svg",
               style = "width:100%"),
      skin = "light",
      status = "primary"
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      sidebarMenu(
        id = "sidebar_tab",
        menuItem("Home", tabName = "home"),
        menuItem("Tutorial", tabName = "tut"),
        menuItem("Input data", tabName = "inp"),
        menuItem("PCA", tabName = "pca"),
        menuItem(p("Table", style = "font-weight : bold; text-decoration: underline;"),
          tabName = "tabl_gene"
        ),
        menuItem("Counts plot", tabName = "countsplot"),
        menuItem("MA-plot", tabName = "maplot"),
        menuItem("Volcano plot", tabName = "volcano"),
        menuItem("Heatmap", tabName = "heatmap")
      ),
      selectInput(
        inputId = "contrast_act",
        label = div("Select the contrast", style = "color : white"),
        choices = NULL,
        selected = NULL
      ),
      tags$a(tags$img(
        src = "logo.svg",
        style = "position:fixed;bottom:0;margin:0 0 15px 25px;",
        alt = "GENOM'IC"
      ),
      href = "https://institutcochin.fr/genomic",
      target = "_blank"
      )
    ),
    body = dashboardBody(
      waiter::useWaiter(),
      tabItems(
        tabItem(
          tabName = "home",
          tabBox(
            title = div("RADISH : a RNA-Seq Analysis Dashboard In Shiny",
              style = "font-weight : bold"
            ),
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
          tabName = "countsplot",
          CountsUI("cnts")
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
  ui
}
