observeEvent(res(), {
  colnames_table <- res() %>% colnames()
    updateCheckboxGroupInput(
        inputId = "genes_columns",
        choices = colnames_table[!(colnames_table %in% base_table_columns)],
    )
})

output$outlier <- renderUI({
  req(res())
  nb_na <- res() %>% 
    filter(if_any(padj, ~ is.na(.x))) %>%
    nrow()
  HTML(paste("<b> ", 
             nb_na,
             "</b>",
             " genes were removed, as they are deemed outliers.",
             "<br>",
             "<br>"))
})

genes_table <- eventReactive({
  input$pval_cutoff
  input$lfc_cutoff
  input$genes_columns
  res()
},{
  res() %>%
    filter(
      padj < input$pval_cutoff,
      log2FoldChange > input$lfc_cutoff |
        log2FoldChange < -input$lfc_cutoff
    ) %>%
    # Risqué : pourrait sélectionner trop largement
    select(all_of(base_table_columns), all_of(input$genes_columns)) %>%
    # Significant digits
    mutate(dplyr::across(where(is.numeric), signif, 3))
})

# Pour téléchargement
sel_genes_table <- eventReactive({
  genes_table()
  # Fourni par DT
  input$genes_rows_selected
},{
  # res, pour avoir toutes les colonnes
  genes_table()[input$genes_rows_selected, ] %>%
    select(Row.names) %>%
    inner_join(res(), by = "Row.names")
})

# For the choice in VP and HM
sel_genes_names <- eventReactive(
  sel_genes_table(),
  {
    sel_genes_table() %>%
      filter(!is.na(symbol)) %>%
      pull(symbol)
})

# For the choice in VP and HM
sel_genes_ids <- eventReactive(
  sel_genes_table(),
  {
    sel_genes_table() %>%
      filter(is.na(symbol)) %>%
      pull(Row.names)
})

select_num <- eventReactive({
  my_values$given_genes
  genes_table()
}, {
  gg_reg <- paste(my_values$given_genes, collapse = "|")
  grep(gg_reg, genes_table()$symbol, ignore.case = TRUE)
})

output$genes <- renderDT(
  {
    req(c(input$pval_cutoff, input$lfc_cutoff))
    req(res())
    genes_table()
  },
  rownames = FALSE,
  # filter = "top", # ne permet pas de sélectionner abs(x) > 1
  class = "cell-border stripe hover order-colum",
  colnames = c("Gene ID" = "Row.names",
               "Adjusted p-value" = "padj",
               "Mean of normalised counts, all samples" = "baseMean",
               "log2(FoldChange)" = "log2FoldChange",
               "Gene name" = "symbol"),
  options = list(scrollX = TRUE)
)

output$download_sel_genes <- downloadHandler(
  filename = function() {
    paste("selected_genes", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(sel_genes_table(), file)
  }
)

output$download_sel_names <- downloadHandler(
  filename = function() {
    paste("selected_genes_names", ".txt", sep = "")
  },
  content = function(file) {
    write(sel_genes_names(), file)
  }
)