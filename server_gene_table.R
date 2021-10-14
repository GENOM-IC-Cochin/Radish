observeEvent(res(), {
    updateCheckboxGroupInput(
        inputId = "genes_columns",
        choices = res() %>% colnames(),
        selected = c("baseMean", "log2FoldChange", "pvalue", "padj", "symbol", "ensembl_gene_id")
    )
})

output$genes <- DT::renderDataTable(
    {
        req(c(input$pval_cutoff, input$lfc_cutoff))
        req(res())
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
    },
    rownames = FALSE,
    options = list(scrollX = TRUE)
)