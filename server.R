server <- function(input, output, session) {
  my_values <- reactiveValues(
    counts = NULL,
    all_results = NULL,
    rld = NULL,
    config = NULL,
    contrastes = NULL
  )
  observeEvent(my_values$all_results, {
    updateSelectInput(
      inputId = "contrast_act",
      choices = names(my_values$all_results)
    )
  })

  source(file = "./utils.R", local = TRUE)
  source(file = "./server_pca.R", local = TRUE)
  source(file = "./server_volcano.R", local = TRUE)
  source(file = "./server_input.R", local = TRUE)
  source(file = "./server_gene_table.R", local = TRUE)
  source(file = "./server_heatmap.R", local = TRUE)
}
