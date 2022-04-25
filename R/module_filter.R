# Module for filtering on padj and/or lfc

#UI ----------------------------------------------------------------------------
FilterUI <- function(id, default) {
  ns <- NS(id)
  uiOutput(ns("filters"))
}


# Server -----------------------------------------------------------------------
FilterServer <- function(id, res, default, reset) {
  stopifnot(is.reactive(res))
  stopifnot(!is.reactive(default))
  stopifnot(is.reactive(reset))
  moduleServer(id, function(input, output, session) {
    
    observeEvent(reset(), {
      if(!is.null(default$pval)) {
        updateSliderTextInput(session = session,
                        inputId = "pval_filter",
                        selected = default$pval)
      }
      if(!is.null(default$lfc)) {
        updateNumericInput(inputId = "lfc_filter",
                     min = 0,
                     value = default$lfc)
      }
    })
    
    res_filtered <- reactive({
      req(res())
      if(!is.null(input$lfc_filter) & !is.null(input$pval_filter)) {
        res_filter(deseq_results = res(),
                   lfc_filter = input$lfc_filter,
                   pval_filter = input$pval_filter)
      } else if(!is.null(input$lfc_filter)) {
        res_filter(deseq_results = res(),
                   lfc_filter = input$lfc_filter)
      } else if(!is.null(input$pval_filter)) {
        res_filter(deseq_results = res(),
                   pval_filter = input$pval_filter)
      }
    })
    
    output$filters <- renderUI({
      # If there is no default pval or lfc specified, don't show the input for it
      # For instance maplot does not need a lfc cut.
      tagList(
        if(!is.null(default$pval)) {
          sliderTextInput(session$ns("pval_filter"),
                          "Select the Adjusted p-value threshold (padj <= ?)",
                          choices = c(0.0001, 0.001, 0.01, 0.05, 0.1, 1),
                          selected = default$pval)
        },
        if(!is.null(default$lfc)) {
          numericInput(session$ns("lfc_filter"),
                       "Select the log(FoldChange) threshold (|lfc| >= ?)",
                       min = 0,
                       value = default$lfc,
                       step = .25)
        }
      )
    })
    
    list(
      res_filtered = res_filtered,
      pval = reactive(input$pval_filter),
      lfc = reactive(input$lfc_filter)
    )
  })
}


# TestApp ----------------------------------------------------------------------
FilterApp <- function() {
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Input",
               InputUI("inp")),
      tabPanel("Filter",
               FilterUI("fil", list("pval" = 0.05)),
               actionButton("reset", "Reset"),
               htmlOutput("nb")
      )
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"))
    filter_res <- FilterServer("fil", list_loaded$res, list("pval" = 0.05), reactive(input$reset))
    output$nb <- renderUI({
      req(filter_res$res_filtered())
      HTML(paste0("Found ",
                  filter_res$res_filtered() %>% filter(sig_expr != "ns") %>% nrow(),
                  " significatively differentially expressed genes"))
    })
  }
  shinyApp(ui, server)
}
