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
        actionButton(ns("pca_button"),
                     "Calculate PCA",
                     class = "btn-warning")
        
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
      status = "warning",
      width = 4,
      selectizeInput(
        inputId = ns("excl_samp"),
        label = "Select outliers to exclude (quite slow)",
        multiple = TRUE,
        choices = NULL,
        selected = NULL,
        options = NULL
      ),
      
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
    box(title = "Download",
        status = "warning",
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
                      contrastes,
                      txi.rsem,
                      rld) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(txi.rsem))
  stopifnot(is.reactive(rld))
  moduleServer(id, function(input, output, session) {
    
    observeEvent(config(), {
      updateSelectizeInput(
        inputId = "excl_samp",
        choices = config()$Name,
        # To forbid PCA plots with two samples
        options = list(maxItems = length(config()$Name) - 3)
      )
    })
    
    data <- eventReactive({
      rld()
      txi.rsem()
      config()
      input$pca_button
    },{
      req(rld(),
          config(),
          txi.rsem())
      ntop <- 500
      rld_pca(rld(), config(), txi.rsem(), input$excl_samp, ntop)
    })
    
    cur_plot <- reactive(
      my_pca(data(), theme = input$theme, show_labels = input$labels)
    )

    output$pca <- renderPlot({
      cur_plot()
    })

    output$scree <- renderPlot({
      req(data())
      data.frame(variance_exp = data()$variance,
                 dimension = as.factor(1:length(data()$variance))) %>%
        ggplot(aes(x = dimension, y = variance_exp)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Dimensions",
             y = "Percentage of variance") +
        geom_text(aes(label = signif(variance_exp, 3)), vjust = 1.6, colour = "white") +
        theme_bw()
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
    tabsetPanel(type = "tabs",
                tabPanel("Input", InputUI("inp")),
                tabPanel("PCA", PcaUI("pca1"))
    )
  )
  
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control")) 
    PcaServer(
      id = "pca1",
      counts = list_loaded$counts,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      txi.rsem = list_loaded$txi.rsem,
      rld = list_loaded$rld
    )
  }
  shinyApp(ui, server)
}
