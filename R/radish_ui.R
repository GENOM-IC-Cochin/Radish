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
    info = "#07856E",
    danger = "#99350F"
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
  ui <- bs4Dash::dashboardPage(
    freshTheme = global_theme,
    header = bs4Dash::dashboardHeader(
      title = tags$img(src = "logo_app_3.svg",
               style = "width:100%"),
      skin = "light",
      status = "primary"
    ),
    sidebar = bs4Dash::dashboardSidebar(
      skin = "light",
      bs4Dash::sidebarMenu(
        id = "sidebar_tab",
        bs4Dash::menuItem("Home", tabName = "home"),
        bs4Dash::menuItem("Tutorial", tabName = "tut"),
        bs4Dash::menuItem("Input data", tabName = "inp"),
        bs4Dash::menuItem("PCA", tabName = "pca"),
        bs4Dash::menuItem(p("Table", style = "font-weight : bold; text-decoration: underline;"),
          tabName = "tabl_gene"
        ),
        bs4Dash::menuItem("Counts plot", tabName = "countsplot"),
        bs4Dash::menuItem("Upset plot", tabName = "upset"),
        bs4Dash::menuItem("MA-plot", tabName = "maplot"),
        bs4Dash::menuItem("Volcano plot", tabName = "volcano"),
        bs4Dash::menuItem("Heatmap", tabName = "heatmap")
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
    body = bs4Dash::dashboardBody(
      waiter::useWaiter(),
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "home",
          bs4Dash::tabBox(
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
        bs4Dash::tabItem(
          tabName = "tut",
          bs4Dash::tabBox(
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
        bs4Dash::tabItem(
          tabName = "inp",
          InputUI("inp")
        ),
        bs4Dash::tabItem(
          tabName = "pca",
          PcaUI("pca")
        ),
        bs4Dash::tabItem(
          tabName = "tabl_gene",
          GeneTableUI("gntab")
        ),
        bs4Dash::tabItem(
          tabName = "countsplot",
          CountsUI("cnts")
        ),
        bs4Dash::tabItem(
           tabName = "upset",
           UpsetUI("up")
        ),
        bs4Dash::tabItem(
          tabName = "maplot",
          MAplotUI("ma")
        ),
        bs4Dash::tabItem(
          tabName = "volcano",
          VolcanoUI("vp")
        ),
        bs4Dash::tabItem(
          tabName = "heatmap",
          HeatmapUI("hm")
        )
      )
    )
  )
  ui
}
