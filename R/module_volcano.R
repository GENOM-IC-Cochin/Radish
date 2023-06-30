# Module for the volcano plot

# UI ---------------------------------------------------------------------------
VolcanoUI <- function(id) {
  # Pour rendre un peu plus court
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::tabBox(
                 width = 12,
                 tabPanel(
                   title = "Static",
                   plotOutput(outputId = ns("volcano_plot")),
                   bs4Dash::actionButton(ns("draw"), "Draw Volcano Plot",
                                         status = "secondary"),
                   bs4Dash::actionButton(ns("reset"),
                                         "Reset defaults",
                                         status = "secondary")
                 ),
                 tabPanel(
                   title = "Interactive",
                   plotly::plotlyOutput(ns("plotly_vp"),
                                        height = "600px")
                 )
              )
    ),
    fluidRow(
      bs4Dash::box(title = "Appearance",
                   status = "info",
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
                   colourpicker::colourInput(
                                   inputId = ns("up_col"),
                                   label = "Choose the color of the upregulated genes",
                                   value = "#fe7f00"
                                 ),
                   colourpicker::colourInput(
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
      bs4Dash::box(title = "Text",
                   status = "info",
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
      bs4Dash::box(title = "Download",
                   status = "info",
                   width = 4,
                   DownloadUI(ns("dw"))
                   )
    )
  )
}


# Server -----------------------------------------------------------------------

VolcanoServer <- function(id,
                          res,
                          contrast_act,
                          contrastes,
                          sel_genes_table) {
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(sel_genes_table))
  moduleServer(id, function(input, output, session){
    
    observeEvent(res(),{
      updateSliderInput(
        inputId = "x_max",
        max = lfc_max_abs(donnees = res()),
        value = lfc_max_abs(donnees = res())
      )
    })
    
    observeEvent(res(), {
      updateSliderInput(
        inputId = "y_max",
        max = log_padj_max(donnees = res()),
        value = log_padj_max(donnees = res())
      )
    })
    
    
    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = res,
      sel_genes_table = sel_genes_table
    )
    
    
    observeEvent(input$reset, {
      colourpicker::updateColourInput(session = session, "up_col", value = "#fe7f00")
      colourpicker::updateColourInput(session = session, "down_col", value = "#007ffe")
      updateSelectInput(inputId = "theme", selected = "Classic")
      updateSliderInput(inputId = "ratio", value = 1)
      updateTextInput(inputId = "up_leg", value = "up")
      updateTextInput(inputId = "down_leg", value = "down")
      updateTextInput(inputId = "ns_leg", value = "ns")
      updateSliderInput(inputId = "lab_size", value = 3)
      req(res())
      updateSliderInput(inputId = "y_max", value = log_padj_max(donnees = res()))
      updateSliderInput(inputId = "x_max", value = lfc_max_abs(donnees = res()))
      req(contrast_act())
      updateTextInput(
        inputId = "plot_title",
        value = paste(" Gene expression change in",
                      contr_str(contrastes(), contrast_act(), sep = " vs "))
      )
    })
    
    
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
    

    plot_data <- eventReactive({
      res()
      input$x_max
      input$y_max
    }, {
      res() %>%
        mutate(outside = case_when(
          -log10(padj) > input$y_max | abs(log2FoldChange) > input$x_max ~ "out",
          TRUE ~ "in"
        ))
    })
    
    cur_plot <- eventReactive(input$draw, {
      req(res())
      my_volcanoplot(
        plot_data = plot_data(),
        titre = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        axis_max = c(input$x_max, input$y_max),
        ratio = input$ratio,
        theme = input$theme,
        selected_genes = c(genes_selected$sel_genes_names(), genes_selected$sel_genes_ids()),
        label_size = input$lab_size
      )
    })
    
    output$volcano_plot <- renderPlot({
      req(cur_plot())
      cur_plot()
    })
    
    output$plotly_vp <- plotly::renderPlotly({
      req(res())
      gg_vp <- my_volcanoplot(
        plot_data = plot_data(),
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        axis_max = c(input$x_max, input$y_max),
        ratio = input$ratio,
        theme = input$theme
      )
      
      gply <- plotly::ggplotly(p = gg_vp,
               tooltip = c("text"),
               dynamicTicks = TRUE,
               height = 600,
               width = 600)
      # Tentative improvements
      plotly::toWebGL(gply)
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
    bs4Dash::tabsetPanel(type = "tabs",
                tabPanel("input", InputUI("inp")),
                tabPanel("Volcano Plot", VolcanoUI("v1"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("1"))
    VolcanoServer(id = "v1",
                  res = list_loaded$res,
                  contrast_act = reactive("1"),
                  contrastes = list_loaded$contrastes,
                  sel_genes_table = reactive(head(list_loaded$res())))
    
  }
  shinyApp(ui, server)
}
