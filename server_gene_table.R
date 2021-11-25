observeEvent(res(), {
  colnames_table <- res() %>% colnames()
    updateCheckboxGroupInput(
        inputId = "genes_columns",
        choices = colnames_table[!(colnames_table %in% base_table_columns)],
    )
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
  res()
  # Fourni par DT
  input$genes_rows_selected
},{
  genes_table()[input$genes_rows_selected, ]
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
  colnames = c('Adjusted p-value' = 'padj',
               "Mean of normalised counts, all samples" = "baseMean",
               "log2(FoldChange)" = "log2FoldChange",
               "Ensembl Gene ID" = "ensembl_gene_id",
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