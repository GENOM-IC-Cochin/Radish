# Module for (ggsave) download

# UI ---------------------------------------------------------------------------
DownloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("format"),
      label = "Format of the downloaded plot",
      choices = c("svg", "png", "pdf"),
      selected = "pdf"
    ),
    downloadButton(
      outputId = ns("down"),
      label = "Download plot"
    )
  )
}


# Server -----------------------------------------------------------------------
DownloadServer <- function(id, cur_plot, plotname, ratio, input, output, session) {
  stopifnot(is.reactive(cur_plot))
  stopifnot(is.reactive(plotname))
  stopifnot(is.reactive(ratio))
  moduleServer(id, function(input, output, session) {
    
    output$down <- downloadHandler(
      filename = function() {
        paste0(plotname(), ".", req(input$format))
      },
      content = function(file) {
        ggsave(file, plot = cur_plot(),
               device = req(input$format),
               height = (7 * (ratio())^0.5),
               width = (7 / (ratio())^0.5),
               dpi = 600)
      }
    )
  })
}


# Test app ---------------------------------------------------------------------
DownloadApp <- function() {
  ui <- fluidPage(
    plotOutput("plot"),
    DownloadUI("dw")
  )
  server <- function(input, output, session) {
    plot_iris <- reactive(
       ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
         geom_point()
    )
    output$plot <- renderPlot(
      {
        plot_iris()
      }
    )
    DownloadServer("dw", plot_iris, reactive("plot"), reactive(1), input, output, session)
  }
  shinyApp(ui, server)
}