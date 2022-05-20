# Module for gene counts

# UI ---------------------------------------------------------------------------

CountsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    bs4Dash::box(title = "Counts",
        width = 12,
        status = "primary",
        plotOutput(ns("counts_plot")),
        bs4Dash::actionButton(ns("draw"), "Draw Plot",
                     status = "secondary")
        ),
    ),
    fluidRow(
      bs4Dash::box(
        title = "Settings",
        status = "secondary",
        width = 4,
        selectInput(ns("variable"),
                    "Select the variable to display",
                    choices = NULL),
        selectInput(ns("levels"),
                    label = "",
                    multiple = TRUE,
                    choices = NULL),
        GeneSelectUI(ns("gnsel")),
        selectInput(
          inputId = ns("log"),
          label = "Log10 y-axis",
          choices = c("Yes" = TRUE, "No" = FALSE)
        ),
        checkboxInput(
          inputId = ns("zero"),
          label = NULL,
          value = FALSE
        )
      ),
      bs4Dash::box(
        title = "Appearance",
        status = "secondary",
        width = 4,
        selectInput(
                    inputId = ns("theme"),
                    label = "Choose the theme for the plot",
                    choices = themes_gg,
                    selected = "Classic with gridlines"
        ),
        sliderInput(
                inputId = ns("ratio"),
                label = "Choose the plot aspect ratio",
                value = 1,
                min = 0.5,
                max = 2
              )
      ),
      bs4Dash::box(
        title = "Download",
        status = "secondary",
        width = 4,
        DownloadUI(ns("dl"))
      )
    )
  )
}


# Server -----------------------------------------------------------------------
CountsServer <- function(id,
                         counts,
                         config,
                         contrastes,
                         sel_genes_table) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(sel_genes_table))

  moduleServer(id, function(input, output, session){

    observeEvent(config(), {
      updateSelectInput(inputId = "variable",
                        choices = config() %>%
                          select(-File, -Name) %>%
                          colnames
                        )
    })

    observeEvent(input$variable, {
      pos_levels <- config() %>%
                          pull(all_of(input$variable)) %>%
                          unique()
      updateSelectInput(inputId = "levels",
                        label = paste("Select the levels of", input$variable, "to display"),
                        choices = pos_levels,
                        selected = pos_levels
                        )
    })

    observeEvent(input$log, {
      if(input$log) {
        updateCheckboxInput(
          inputId = "zero",
          label = "Include 0.5 in the y scale",
          value = FALSE
        )
      } else {
      updateCheckboxInput(
          inputId = "zero",
          label = "Include 0 in the y scale",
          value = FALSE
      )
      }
    })

    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = counts,
      sel_genes_table = sel_genes_table
    )

    cur_plot <- eventReactive(input$draw, {
      req(counts(),
          config(),
          input$levels,
          input$variable)
      plot_data <- counts() %>%
        filter(symbol %in% genes_selected$sel_genes_names() | Row.names %in% genes_selected$sel_genes_ids()) %>%
        mutate(Row.names = coalesce(symbol, Row.names)) %>%
        select(all_of(c("Row.names", config()[, "Name"])))
      validate(need(nrow(plot_data) > 0, "Select at least one gene"))
      my_counts_plot(
        plot_data = plot_data,
        variable = input$variable,
        logy = input$log,
        levels = input$levels,
        config = config(),
        zero = input$zero,
        ratio = input$ratio,
        theme = input$theme
        )
    })


    output$counts_plot <- renderPlot({
      cur_plot()
    })

    DownloadServer(
      id = "dl",
      cur_plot = cur_plot,
      plotname = reactive("count_plot"),
      ratio = reactive(1)
    )

  })
}



# Test App ---------------------------------------------------------------------

CountsApp <- function() {
  ui <- fluidPage(
    bs4Dash::tabsetPanel(type = "tabs",
                tabPanel("input", InputUI("inp")),
                tabPanel("Volcano Plot", CountsUI("counts"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("1"))
    CountsServer(id = "counts",
                 counts = list_loaded$counts,
                  config = list_loaded$config,
                  contrastes = list_loaded$contrastes,
                  sel_genes_table = reactive(head(list_loaded$res())))

  }
  shinyApp(ui, server)
}
