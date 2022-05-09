# Module for PCA


# UI ---------------------------------------------------------------------------
PcaUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tabBox(
      width = 12,
      status = "primary",
      tabPanel(
        title = "PCA plot",
        plotOutput(ns("pca")),
        actionButton(ns("draw"),
                     "Draw PCA",
                     status = "secondary")
        
      ),
      tabPanel(
        title = "Screeplot",
        plotOutput(ns("scree"))
      )
    )
  ),
  fluidRow(
    box(
      title = "Settings",
      status = "secondary",
      width = 4,
      selectizeInput(
        inputId = ns("excl_samp"),
        label = "Select outliers to exclude (quite slow)",
        multiple = TRUE,
        choices = NULL,
        selected = NULL,
        options = NULL
      ),
      actionButton(
        inputId = ns("recomp_pca"),
        label = "Recompute PCA",
        status = "secondary"
      ),
      br(),
      selectInput(
        inputId = ns("theme"),
        label = "Choose the theme for the plot",
        choices = themes_gg,
        selected = "Classic"
      ),
      checkboxInput(ns("labels"),
                    "Should samples be labeled",
                    TRUE)
    ),
    box(
      title = "Colors",
      status = "secondary",
      width = 4,
      uiOutput(ns("colors"))
    ),
    box(
      title = "Download",
      status = "secondary",
      width = 4,
      DownloadUI(ns("dw"))
    )
  )
  )
}


# Server -----------------------------------------------------------------------

PcaServer <- function(id,
                      counts,
                      config,
                      txi.rsem,
                      rld) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(txi.rsem))
  stopifnot(is.reactive(rld))
  moduleServer(id, function(input, output, session) {

    data <- reactiveVal()

    observeEvent(config(), {
      updateSelectizeInput(
        inputId = "excl_samp",
        choices = config()$Name,
        # To forbid PCA plots with two samples
        options = list(maxItems = length(config()$Name) - 3)
      )
    })

    observeEvent(
      {
        rld()
        txi.rsem()
        config()
        input$recomp_pca
      },
      {
        req(
          rld(),
          config(),
          txi.rsem()
        )
        ntop <- 500
        data(rld_pca(rld(), config(), txi.rsem(), input$excl_samp, ntop))
      },
      ignoreNULL = FALSE
    )

    cur_plot <- eventReactive(input$draw, {
      color_by_cond <- set_names(
        map_chr(
          uniq_conds(),
          ~ input[[.x]] %||% ""
        ),
        uniq_conds()
      )
      color_by_cond[color_by_cond == ""] <- NA
      cur_conds <- color_by_cond[data()$data$Condition %>% unique]
      my_pca(data(),
        theme = input$theme,
        show_labels = input$labels,
        if (anyNA(cur_conds)) {
          NULL
        } else {
          cur_conds
        }
      )
    })

    output$pca <- renderPlot({
      cur_plot()
    })

    output$scree <- renderPlot({
      req(data())
      data.frame(
        variance_exp = data()$variance,
        dimension = as.factor(1:length(data()$variance))
      ) %>%
        ggplot(aes(x = dimension, y = variance_exp)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(
          x = "Dimensions",
          y = "Percentage of variance"
        ) +
        geom_text(aes(label = signif(variance_exp, 3)), vjust = 1.6, colour = "white") +
        theme_bw()
    })

    uniq_conds <- reactive(config()$Condition %>% unique() %>% as.character())

    output$colors <- renderUI({
      map2(
        uniq_conds(),
        hue_pal()(length(uniq_conds())),
        ~ colourInput(
          inputId = session$ns(.x),
          paste("Choose the color of : ", .x),
          value = .y
        )
      )
    })

    DownloadServer(
      id = "dw",
      cur_plot = cur_plot,
      plotname = reactive("pcaplot"),
      ratio = reactive(1)
    )
  })
}

# Test App ---------------------------------------------------------------------
PcaApp <- function() {
  ui <- fluidPage(
    tabsetPanel(
      type = "tabs",
      tabPanel("Input", InputUI("inp")),
      tabPanel("PCA", PcaUI("pca1"))
    )
  )

  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("1"))
    PcaServer(
      id = "pca1",
      counts = list_loaded$counts,
      config = list_loaded$config,
      txi.rsem = list_loaded$txi.rsem,
      rld = list_loaded$rld
    )
  }
  shinyApp(ui, server)
}
