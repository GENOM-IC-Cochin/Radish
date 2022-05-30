# Module for PCA


# UI ---------------------------------------------------------------------------
PcaUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tabBox(
      width = 12,
      tabPanel(
        title = "PCA plot",
        plotOutput(ns("pca")),
        bs4Dash::actionButton(ns("draw"),
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
    bs4Dash::box(
      title = "Settings",
      status = "info",
      width = 4,
      selectizeInput(
        inputId = ns("excl_samp"),
        label = "Select outliers to exclude (quite slow)",
        multiple = TRUE,
        choices = NULL,
        selected = NULL,
        options = NULL
      ),
      bs4Dash::actionButton(
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
                    "Label samples",
                    TRUE),
      selectInput(ns("color_by"),
                  "Color samples by",
                  choices = NULL),
      uiOutput(ns("shape"))
    ),
    bs4Dash::box(
      title = "Colors",
      status = "info",
      width = 4,
      uiOutput(ns("colors"))
    ),
    bs4Dash::box(
      title = "Download",
      status = "info",
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

    observeEvent(config(), {
      freezeReactiveValue(input, "color_by")
      updateSelectInput(
        inputId = "color_by",
        choices = config() %>%
          select(-File, -Name) %>%
          colnames()
      )
    })


    output$shape <- renderUI({
      #if there is more than 1 variable
      req(config(),
          input$color_by)
      if(ncol(config()) > 3){
        tagList(
          checkboxInput(
            session$ns("shape"),
            "Use shape to display second variable",
            FALSE),
          selectInput(
            session$ns("shape_by"),
            "Shape samples by",
            choices = config() %>%
              select(-all_of(c("File", "Name", input$color_by))) %>%
              colnames()
          )
        )
      }
    })


    # Observer to get the data on input change
    observeEvent(
      {
        config()
      },
      {
        req(
          rld(),
          config(),
          txi.rsem()
        )
        ntop <- 500
        # No sample is excluded on input change
        data(rld_pca(rld(), config(), txi.rsem(), NULL, ntop))
      }
    )
    
    # Observer for recomputation
    observeEvent(
      {
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
      }
    )


    levels <- eventReactive(input$color_by, {
      req(input$color_by,
          config())
      config() %>%
        pull(all_of(input$color_by)) %>%
        unique() %>%
        as.character()
    })


    cur_plot <- eventReactive(input$draw, {
      req(levels(),
          data())
      
      req(purrr::map_chr(
          levels(),
          ~ input[[.x]] %||% ""
        ))
      color_by_level <- magrittr::set_names(
        purrr::map_chr(
          levels(),
          ~ input[[.x]] %||% ""
        ),
        levels()
      )
      color_by_level[color_by_level == ""] <- NA
      cur_levels <- color_by_level[data()$data %>% pull(input$color_by) %>% unique]
      my_pca(data(),
        theme = input$theme,
        show_labels = input$labels,
        color_by_level = if (anyNA(cur_levels)) {NULL} else {cur_levels},
        color_by = input$color_by,
        # "none" if input$shape does not exist OR if input$shape_by is not checked
        shape_by = if(!is.null(input$shape)) {if(input$shape) {input$shape_by} else {"none"}} else {"none"}
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


    output$colors <- renderUI({
      req(levels())
      purrr::map2(
        levels(),
        scales::hue_pal()(length(levels())),
        ~ colourpicker::colourInput(
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
    bs4Dash::tabsetPanel(
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
