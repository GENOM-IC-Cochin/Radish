# Module for the volcano plot

# UI ---------------------------------------------------------------------------
VolcanoUI <- function(id) {
  # Pour rendre un peu plus court
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Volcano Plot",
          status = "primary",
          width = 12,
          plotOutput(outputId = ns("volcano_plot")),
          actionButton(ns("draw"), "Draw Volcano Plot",
                       class = "btn-warning"),
          actionButton(ns("reset"),
                       "Reset defaults",
                       class = "btn-warning")
      )
    ),
    fluidRow(
      box(title = "Aesthetics",
          status = "orange",
          width = 4,
          sliderInput(
            inputId = ns("x_max"),
            label = "Maximum value of the x axis",
            min = 0,
            max = 100,
            value = 10
          ),
          sliderInput(
            inputId = ns("y_max"),
            label = "Maximum value of the y axis",
            min = 0,
            max = 100,
            value = 10
          ),
          FilterUI(ns("fil"), list("pval" = 0.05, "lfc" = 1)),
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
          GeneSelectUI(ns("gnsel")),
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
          DownloadUI(ns("dw"))
      )
    )
  )
}


# Server -----------------------------------------------------------------------

VolcanoServer <- function(id,
                          counts,
                          res,
                          config,
                          contrastes,
                          contrast_act,
                          sel_genes_table) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(sel_genes_table))
  moduleServer(id, function(input, output, session){
    
    observeEvent(res(),{
      updateSliderInput(
        inputId = "x_max",
        max = x_max_abs(donnees = res()),
        value = x_max_abs(donnees = res())
      )
    })
    
    observeEvent(res(), {
      updateSliderInput(
        inputId = "y_max",
        max = y_max(donnees = res()),
        value = y_max(donnees = res())
      )
    })
    
    
    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = res,
      sel_genes_table = sel_genes_table
    )
    
    
    observeEvent(input$reset, {
      updateColourInput(session = session, "up_col", value = "#fe7f00")
      updateColourInput(session = session, "down_col", value = "#007ffe")
      updateSelectInput(inputId = "theme", selected = "Classic")
      updateSliderInput(inputId = "ratio", value = 1)
      updateTextInput(inputId = "up_leg", value = "up")
      updateTextInput(inputId = "down_leg", value = "down")
      updateTextInput(inputId = "ns_leg", value = "ns")
      updateSliderInput(inputId = "lab_size", value = 3)
      req(res())
      updateSliderInput(inputId = "y_max", value = y_max(donnees = res()))
      updateSliderInput(inputId = "x_max", value = x_max_abs(donnees = res()))
      req(contrast_act())
      contr <- strsplit(contrast_act(), "_") %>% unlist()
      updateTextInput(
        inputId = "plot_title",
        value = paste(c("Gene expression change in", contr),
                      collapse = " ")
      )
    })
    
    
    observeEvent(contrast_act(), {
      contr <- strsplit(contrast_act(), "_") %>% unlist()
      updateTextInput(
        inputId = "plot_title",
        value = paste(c("Gene expression change in", contr),
                      collapse = " ")
      )
    })
    
    filter_res <- FilterServer("fil",
                               res,
                               list("pval" = 0.05, "lfc" = 1),
                               reactive(input$reset))
    
    cur_plot <- eventReactive(input$draw, {
      req(filter_res)
      my_volcanoplot(
        plot_data = filter_res$res_filtered(),
        titre = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        axis_max = c(input$x_max, input$y_max),
        ratio = input$ratio,
        theme = input$theme,
        selected_genes = c(genes_selected$sel_genes_names(), genes_selected$sel_genes_ids()),
        label_size = input$lab_size,
        lfc_cutoff = filter_res$lfc(),
        pval_cutoff = filter_res$pval()
      )
    })
    
    output$volcano_plot <- renderPlot({
      cur_plot()
    })
    
    DownloadServer(
      id = "dw",
      cur_plot = cur_plot,
      plotname = reactive("volcano_plot"),
      ratio = reactive(input$ratio)
    )
  })
}


# Test App ---------------------------------------------------------------------
VolcanoApp <- function() {
  ui <- fluidPage(
    tabsetPanel(type = "tabs",
    tabPanel("input", InputUI("inp")),
    tabPanel("Volcano Plot", VolcanoUI("v1"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"))
    VolcanoServer(id = "v1",
                  counts = list_loaded$counts,
                  res = list_loaded$res,
                  config = list_loaded$config,
                  contrastes = list_loaded$contrastes,
                  contrast_act = reactive("Cond1_vs_Control"),
                  sel_genes_table = reactive(head(list_loaded$res())))
    
  }
  shinyApp(ui, server)
}