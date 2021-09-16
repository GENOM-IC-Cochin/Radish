server <- function(input, output, session) {
    source(file = "./server_volcano.R", local = TRUE)
    source(file = "./server_input.R", local = TRUE)
    source(file = "./server_gene_table.R", local = TRUE)
}
