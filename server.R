server <- function(input, output, session) {
  source(file = "./utils.R", local = TRUE)
  source(file = "./server_volcano.R", local = TRUE)
  source(file = "./server_input.R", local = TRUE)
  source(file = "./server_gene_table.R", local = TRUE)
  source(file = "./server_heatmap.R", local = TRUE)
}
