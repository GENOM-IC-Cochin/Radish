# Maplot module

# UI ---------------------------------------------------------------------------
MAplotUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
          box(title = "MA-Plot",
              status = "primary",
              width = 12,
              plotOutput(outputId = ns("plot")),
              actionButton(ns("draw"), "Draw MA-Plot",
                           class = "btn-warning")
          )
        ),
        fluidRow(
          box(title = "Aesthetics",
              status = "orange",
              width = 4,
              sliderTextInput(
                inputId = ns("pval_cut"),
                label = "Adjusted pvalue limit for significance",
                choices = c(0.0001, 0.001, 0.01, 0.05, 0.1),
                selected = 0.05
              ),
              colourInput(
                inputId = ns("up_col"),
                label = "Choose the color of the upregulated genes",
                value = "#fe7f00"
              ),
              colourInput(
                inputId = ns("down_col"),
                label = "Choose the color of the downregulated genes",
                value = "#007ffe"
              ),
              selectInput(
                inputId = ns("theme"),
                label = "Choose the theme for the plot",
                choices = themes_gg,
                selected = "Classic"
              ),
              sliderInput(
                inputId = ns("ratio"),
                label = "Choose the plot aspect ratio",
                value = 1,
                min = 0.5,
                max = 2
              )
          ),
          box(title = "Text",
              status = "orange",
              width = 4,
              textInput(
                inputId = ns("plot_title"),
                label = "Title of the plot",
                value = "Gene expression change"
              ),
              textInput(
                inputId = ns("up_leg"),
                label = "Choose the upregulated legend name",
                value = "up"
              ),
              textInput(
                inputId = ns("down_leg"),
                label = "Choose the downregulated legend name",
                value = "down"
              ),
              textInput(
                inputId = ns("ns_leg"),
                label = "Choose the nonsignificant legend name",
                value = "ns"
              ),
              selectizeInput(
                inputId = ns("sel_gene_nm"),
                label = "Select which significant genes (by name) to highlight :",
                choices = NULL,
                multiple = TRUE
              ),
              selectizeInput(
                inputId = ns("sel_gene_id"),
                label = "Select which significant genes (by id) to highlight :",
                choices = NULL,
                multiple = TRUE
              ),
              sliderInput(
                inputId = ns("lab_size"),
                label = "Choose the size of the labels",
                value = 3,
                min = 1,
                max = 4,
                step = .25
              )
          ),
          box(title = "Download",
              status = "orange",
              width = 4,
              selectInput(
                inputId = ns("format"),
                label = "Format of the dowloaded plot",
                choices = c("svg", "png", "pdf"),
                selected = "pdf"
              ),
              downloadButton(
                outputId = ns("down"),
                label = "Download plot"
              )
          )
        )
      )
}


# Server -----------------------------------------------------------------------
MAplotServer <- function(id,
                         counts,
                         res,
                         config,
                         contrastes,
                         contrast_act,
                         sel_genes_names,
                         sel_genes_ids,
                         input,
                         output,
                         session) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(sel_genes_ids))
  stopifnot(is.reactive(sel_genes_names))
  moduleServer(id, function(input, output, session){
    
    observeEvent({
      data()
      sel_genes_names()
    }, {
      updateSelectizeInput(
        inputId = "sel_gene_nm",
        choices = data() %>%
          pull(symbol),
        server = TRUE,
        selected = sel_genes_names()
      )
    })
    
    observeEvent({
      data()
      sel_genes_ids()
    }, {
      updateSelectizeInput(
        inputId = "sel_gene_id",
        choices = data() %>%
          pull(Row.names),
        server = TRUE,
        selected = sel_genes_ids()
      )
    })
    
    observeEvent(res(), {
      contr <- strsplit(contrast_act(), "_") %>% unlist()
      updateTextInput(
        inputId = "plot_title",
        value = paste(c("Gene expression change in", contr),
                      collapse = " ")
      )
    })
    
    data <- reactive({
      # collé ici pour ne pas le recalculer,
      #à chaque modif des paramètres de ma_plot
      req(input$pval_cut)
      res_ma(req(res()),
             pval_cutoff = input$pval_cut)
    })
    
    plot <- eventReactive(input$draw, {
      req(data())
      my_maplot(
        plot_data = data(),
        title = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        ratio = input$ratio,
        selected_genes = c(input$sel_gene_nm, input$sel_gene_id),
        theme = input$theme,
        label_size = input$lab_size,
        pval_cutoff = req(input$pval_cut) 
      )
    })
    
    output$plot <- renderPlot({
      plot()
    })
    
    output$down <- downloadHandler(
      filename = function() {
        paste0("ma_plot.", req(input$format))
      },
      content = function(file) {
        ggsave(file, plot = req(plot()),
               device = req(input$format),
               height = (3.5 + 3.5 * input$ratio),
               width = (3.5 + 3.5 / input$ratio),
               dpi = 600)
      }
    )
    
  })
}
    


# Test App ---------------------------------------------------------------------
MAplotApp <- function() {
  ui <- fluidPage(
    tabsetPanel(type = "tabs",
    tabPanel("input", InputUI("inp")),
    tabPanel("Maplot", MAplotUI("maplot1"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"), input, output, session) 
    MAplotServer(id = "maplot1",
                  counts = list_loaded$counts,
                  res = list_loaded$res,
                  config = list_loaded$config,
                  contrastes = list_loaded$contrastes,
                  contrast_act = reactive("Cond1_vs_Control"),
                  sel_genes_names = reactive(c()),
                  sel_genes_ids = reactive(c()),
                  input = input,
                  output = output,
                  session = session)
    
  }
  shinyApp(ui, server)
}