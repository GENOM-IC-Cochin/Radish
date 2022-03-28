# Module for gene selection (labeling in plots)

# UI ---------------------------------------------------------------------------
GeneSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    )
  )
}


# Server -----------------------------------------------------------------------
GeneSelectServer <- function(
  id,
  src_table, # Either counts or res, depending on the plot
  sel_genes_names,
  sel_genes_ids,
  input,
  output,
  session
) {
  stopifnot(is.reactive(src_table))
  stopifnot(is.reactive(sel_genes_ids))
  stopifnot(is.reactive(sel_genes_names))
  
  moduleServer(id, function(input, output, session) {
     
    observeEvent({
      # Executes, even though no genes is currently selected
      c(src_table(), sel_genes_names())
    }, {
      updateSelectizeInput(
        inputId = "sel_gene_nm",
        choices = src_table() %>%
          pull(symbol),
        server = TRUE,
        selected = sel_genes_names()
      )
    })
    
    observeEvent({
      # Executes, even though no genes is currently selected
      c(src_table(), sel_genes_ids())
    }, {
      updateSelectizeInput(
        inputId = "sel_gene_id",
        choices = src_table() %>%
          pull(Row.names),
        server = TRUE,
        selected = sel_genes_ids()
      )
    })
    
    list(
      sel_genes_ids = reactive(input$sel_gene_id),
      sel_genes_names = reactive(input$sel_gene_nm)
    )
  })
}


# TestApp ----------------------------------------------------------------------
GeneSelectApp <- function() {
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Input", InputUI("inp")),
      tabPanel("Gene Select",
               GeneSelectUI("gs"),
               box(title = "Output",
                   width = 12,
                   verbatimTextOutput("genes"))
      )
    )
  )
  
  server <- function(input, output, server) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"), input, output, session)
    genes_selected <- GeneSelectServer(
      id = "gs",
      src_table = list_loaded$res,
      sel_genes_names = reactive(c()),
      sel_genes_ids = reactive(c()),
      input = input,
      output = output,
      session = session
    )
    output$genes <- renderPrint(
      c(genes_selected$sel_genes_names(), genes_selected$sel_genes_ids())
    )
  }
  shinyApp(ui, server)
}