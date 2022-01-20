output$outlier <- renderUI({
  req(res())
  nb_na <- res() %>% 
    filter(if_any(padj, ~ is.na(.x))) %>%
    nrow()
  HTML(paste("<b>", 
             nb_na,
             "</b>",
             "genes had their <i> p-values </i> set to NA, as they are deemed outliers.",
             "<br>",
             "<br>"))
})

output$sig_genes <- renderUI({
  req(res())
  n_sig <- res() %>%
    filter(padj < 0.05, abs(log2FoldChange) > 1) %>%
    nrow()
  HTML(paste("<b>", n_sig, "</b>",
             "genes are significantly differentially expressed",
             "at an adjusted <i> pvalue </i> of 0.05 and a minimum log2(Foldchange) of 1.",
             "<br>"))
})

genes_table <- eventReactive({
  res()
},{
  res() %>%
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

sel_genes_ids <- eventReactive(
  sel_genes_table(),
  {
    sel_genes_table() %>%
      pull(Row.names)
})

output$read_items <- renderUI({
  names <- ids <- NULL
  if(!is.null(input$given_genes_names)) {
    names <- scan(input$given_genes_names$datapath,
                  what = character())
  }
  if(!is.null(input$given_genes_ids)) {
    ids <- scan(input$given_genes_ids$datapath,
                  what = character())
  }
    HTML("<b>",
         paste(length(ids) + length(names)),
         "</b>",
         "items were read.",
         "<br>",
         "<br>")
})


# Matches the genes given and the rows in the table
observeEvent({
  # either one or the other
  # both NULL stops observeEvent
  c(input$given_genes_names, input$given_genes_ids)
  genes_table()
},
{
  # give priority to ids
  if(!is.null(input$given_genes_ids)) {
    extension <- tools::file_ext(input$given_genes_ids$name)
    validate(need(extension == "txt", "Please upload a plain text (txt) file"))
    
    gene_ids <- scan(file = input$given_genes_ids$datapath,
                     what = character())
    my_values$given_genes_rows <- which(genes_table()$Row.names %in% gene_ids)
    
  }
  if(!is.null(input$given_genes_names)) {
    extension <- tools::file_ext(input$given_genes_names$name)
    validate(need(extension == "txt", "Please upload a plain text (txt) file"))
    
    gene_names <- scan(file = input$given_genes_names$datapath,
                       what = character())
    gg_reg <- paste(gene_names, collapse = "|")
    my_values$given_genes_rows <- c(my_values$given_genes_rows,
                                    grep(gg_reg, 
                                         genes_table()$symbol,
                                         ignore.case = TRUE)) %>% unique()
  }
}
)

cols_to_hide <- eventReactive(res(),{
  # - 1 because JS indices start at 0
  which(!(colnames(res()) %in% base_table_columns)) - 1
})

proxy <- dataTableProxy("genes")

observeEvent(input$select_genes, {
  proxy %>% selectRows(my_values$given_genes_rows)
})

observeEvent(input$clear, {
  proxy %>% selectRows(NULL)
})

output$genes <- renderDT(
  expr = {
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
               "Gene name" = "symbol",
               "Gene description" = "description"),
  extensions = "Buttons",
  options = list(scrollX = TRUE,
                 dom = "Bfrtip",
                 columnDefs = list(
                   list(targets = c(0, 1, 2, 6, 8), className = "noVis"),
                   list(targets = cols_to_hide(), visible = FALSE)
                 ),
                 buttons = list(
                   list(extend = 'colvis', columns = I(':not(.noVis)'))
                   )),
  selection = list(target = "row")
)

output$n_selected <- renderUI({
  HTML(paste("<b>", length(input$genes_rows_selected), "</b>",
             "rows are currently selected. <br> <br>"))
})

output$genes_selected <- renderDT(
  expr = {
    req(sel_genes_table())
    sel_genes_table() %>%
      mutate(dplyr::across(where(is.numeric), signif, 3))
  },
  rownames = FALSE,
  class = "cell-border stripe hover order-colum",
  colnames = c("Gene ID" = "Row.names",
               "Adjusted p-value" = "padj",
               "Mean of normalised counts, all samples" = "baseMean",
               "log2(FoldChange)" = "log2FoldChange",
               "Gene name" = "symbol",
               "Gene description" = "description"),
  extensions = "Buttons",
  options = list(scrollX = TRUE,
                 dom = "Bfrtip",
                 columnDefs = list(
                   list(targets = c(0, 1, 2, 6, 8), className = "noVis"),
                   list(targets = cols_to_hide(), visible = FALSE)
                 ),
                 buttons = list(
                   list(extend = 'colvis', columns = I(':not(.noVis)'))
                   )),
  selection = "none"
)

output$download_sel_genes <- downloadHandler(
  filename = function() {
    paste("selected_genes", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(sel_genes_table(), file)
  }
)

output$download_sel_ids <- downloadHandler(
  filename = function() {
    paste("selected_genes_ids", ".txt", sep = "")
  },
  content = function(file) {
    write(sel_genes_ids(), file)
  }
)