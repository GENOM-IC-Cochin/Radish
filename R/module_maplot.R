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
                           status = "secondary"),
              actionButton(ns("reset"), "Reset defaults",
                           status = "secondary")
          )
        ),
        fluidRow(
          box(title = "Appearance",
              status = "secondary",
              width = 4,
              FilterUI(ns("fil"), list("pval" = 0.05)),
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
              status = "secondary",
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
              status = "secondary",
              width = 4,
              DownloadUI(ns("dw")) 
          )
        )
      )
}


# Server -----------------------------------------------------------------------
MAplotServer <- function(id,
                         res,
                         config,
                         contrast_act,
                         contrastes,
                         sel_genes_table) {
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(sel_genes_table))
  moduleServer(id, function(input, output, session){
    
    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = res,
      sel_genes_table = sel_genes_table
    )
    
    observeEvent({
      contrast_act()
      contrastes()
    }, {
      updateTextInput(
        inputId = "plot_title",
        value = paste(" Gene expression change in",
                      contr_str(contrastes(), contrast_act(), sep = " vs "))
      )
    })
    
    
    observeEvent(input$reset, {
      updateColourInput(session = session, "up_col", value = "#fe7f00")
      updateColourInput(session = session, "down_col", value = "#007ffe")
      updateSelectInput(inputId = "theme", selected = "Classic")
      updateSliderInput(inputId = "ratio", value = 1)
      updateTextInput(inputId = "up_leg", value = "up")
      updateTextInput(inputId = "down_leg", value = "down")
      updateTextInput(inputId = "ns_leg", value = "ns")
      updateSliderInput(inputId = "lab_size", value = 3)
      req(contrast_act())
      updateTextInput(
        inputId = "plot_title",
        value = paste(" Gene expression change in",
                      contr_str(contrastes(), contrast_act(), sep = " vs "))
      )
    })
    
    filter_res <- FilterServer("fil", res, list("pval" = 0.05), reactive(input$reset))
    
    cur_plot <- eventReactive(input$draw, {
      req(filter_res$res_filtered())
      my_maplot(
        plot_data = filter_res$res_filtered(),
        title = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        ratio = input$ratio,
        selected_genes = c(genes_selected$sel_genes_names(),
                           genes_selected$sel_genes_ids()),
        theme = input$theme,
        label_size = input$lab_size
      )
    })
    
    output$plot <- renderPlot({
      cur_plot()
    })
    
    DownloadServer(
      id = "dw",
      cur_plot = cur_plot,
      plotname = reactive("MA_plot"),
      ratio = reactive(input$ratio)
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
    list_loaded <- InputServer("inp", reactive("1"))
    MAplotServer(id = "maplot1",
                  res = list_loaded$res,
                  config = list_loaded$config,
                  contrast_act = reactive("1"),
                 contrastes = list_loaded$contrastes,
                  sel_genes_table = reactive(data.frame(head(list_loaded$res()))))
    
  }
  shinyApp(ui, server)
}
