observeEvent(res(), {
    updateCheckboxGroupInput(
        inputId = "genes_columns",
        choices = res() %>% colnames(),
        selected = c("baseMean", "log2FoldChange", "pvalue", "padj", "symbol", "ensembl_gene_id")
    )
})

genes_table <- eventReactive({
  input$pval_cutoff
  input$lfc_cutoff
  res()
},{
  res() %>%
    filter(
      padj < input$pval_cutoff,
      log2FoldChange > input$lfc_cutoff |
        log2FoldChange < -input$lfc_cutoff
    ) %>%
    # Risqué : pourrait sélectionner trop largement
    select(all_of(input$genes_columns)) %>%
    # Significant digits
    mutate(dplyr::across(where(is.numeric), signif, 3))
})

sel_genes_table <- eventReactive({
  res()
  # Fourni par DT
  input$genes_rows_selected
},{
  genes_table()[input$genes_rows_selected, ]
})

output$genes <- DT::renderDataTable(
    {
        req(c(input$pval_cutoff, input$lfc_cutoff))
        req(res())
        genes_table()
    },
    rownames = FALSE,
    # filter = "top", # ne permet pas de sélectionner abs(x) > 1
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