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
      status = "orange",
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
      )
    ),
    box(title = "Download",
        status = "orange",
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
                      rld,
                      input,
                      output,
                      session) {
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
      # Exclude samples
      if(!is.null(input$excl_samp)) {
        withProgress(message = "Recalculating...",{
          drop_samp <- which((colnames(txi.rsem()$counts)) %in% input$excl_samp)
          rld_tr <- recalculate_rld_pca(txi.rsem(), drop_samp, config()) 
        })
      } else {
        rld_tr <- rld()
      }
      
      rv <- rowVars(assay(rld_tr))
      selected_genes <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
      mat <- t(assay(rld_tr)[selected_genes, ])
      pc <- prcomp(mat)
      eig <- (pc$sdev)^2
      variance <- eig*100/sum(eig)
      
      PCAdata<-as.data.frame(pc$x)
      # Join with condition, on name, to be sure of matches between condition and sample
      PCAdata <- PCAdata %>%
        rownames_to_column(var = "Name") %>%
        inner_join(config(), by = "Name") %>%
        select(-File) %>%
        column_to_rownames(var = "Name")
      list("data" = PCAdata, "variance" = variance)
    })
    
    cur_plot <- reactive(
      my_pca(data(), theme = input$theme)
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
      ratio = reactive(1),
      input = input,
      output = output,
      session = session
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
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"), input, output, session) 
    PcaServer(
      id = "pca1",
      counts = list_loaded$counts,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      txi.rsem = list_loaded$txi.rsem,
      rld = list_loaded$rld,
      input = input,
      output = output,
      session = session
    )
  }
  shinyApp(ui, server)
}