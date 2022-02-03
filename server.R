server <- function(input, output, session) {
  my_values <- reactiveValues(
    given_genes_rows = NULL
  )
  observeEvent(all_results(), {
    updateSelectInput(
      inputId = "contrast_act",
      choices = names(all_results())
    )
  })

  output$disp_contr <- renderText({
    input$contrast_act
  })

  source(file = "./utils.R", local = TRUE)
  source(file = "./server_pca.R", local = TRUE)
  source(file = "./server_volcano.R", local = TRUE)
  source(file = "./server_input.R", local = TRUE)
  source(file = "./server_gene_table.R", local = TRUE)
  source(file = "./server_heatmap.R", local = TRUE)
}
