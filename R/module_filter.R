# Module for filtering on padj and/or lfc

#UI ----------------------------------------------------------------------------
FilterUI <- function(id, default) {
  ns <- NS(id)
  tagList(
    if(!is.null(default$pval)) {
      sliderTextInput(ns("pval_filter"),
                      "Select the Adjusted p-value filter (padj <= ?)",
                      choices = c(0.0001, 0.001, 0.01, 0.05, 0.1, 1),
                      selected = default$pval)
    },
    if(!is.null(default$lfc)) {
      numericInput(ns("lfc_filter"),
                   "Select the log(FoldChange) filter (|lfc| >= ?)",
                   min = 0,
                   value = default$lfc)
    }
  )
}


# Server -----------------------------------------------------------------------
FilterServer <- function(id, res) {
  stopifnot(is.reactive(res))
  moduleServer(id, function(input, output, session) {
    res_filtered <- reactive({
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
               htmlOutput("nb")
      )
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"))
    filter_res <- FilterServer("fil", list_loaded$res)
    output$nb <- renderUI({
      browser()
      HTML(paste0("Found ",
                  filter_res$res_filtered() %>% filter(sig_expr != "ns") %>% nrow(),
                  " significatively differentially expressed genes"))
    })
  }
  shinyApp(ui, server)
}