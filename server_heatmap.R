condition_possibles <- reactive({
  condition <- conf_file()$Condition %>% unique() 
  if(input$top_gene) {
    condition[condition %in% contrast_actuel()]
  } else {
    condition
  }
})

observeEvent(input$top_gene,
             updateTabsetPanel(
               inputId = "settings",
               selected = ifelse(input$top_gene,
                                 "diff",
                                 "all")
             )
)

observe({
  updateCheckboxInput(
    inputId = "top_gene",
    label = paste("I want the heatmap to contain the top significant genes",
                  "from the current contrast", contrast_actuel()[1], "vs",
                  contrast_actuel()[2])
  )
})

observe(
  updateCheckboxGroupInput(
    inputId = "sel_cond",
    choices = condition_possibles()
  )
)

observe(
  updateNumericInput(
    inputId = "nb_top_gene",
    max = nrow(req(counts))
  )
)

observe({
  req(counts())
  updateSelectizeInput(
    inputId = "sel_gene_hm",
    choices = as.vector(counts()$symbol),
    server = TRUE
  )
})


heatmap_data <- eventReactive(input$draw_hm, {
  req(input$sel_cond, conf_file(), counts())
  # sélection du nom des échantillons
  echantillons <- conf_file() %>%
    filter(Condition %in% input$sel_cond) %>%
    pull(Name)
  if(input$top_gene) {
    # Si l'on veut que les plus différentiellement exprimés
    res() %>%
      filter(log2FoldChange > 1 | log2FoldChange < -1) %>%
      slice_min(order_by = padj, n = input$nb_top_gene) %>%
      select(echantillons) %>%
      as.matrix()
  } else {
    # Si l'on veut sélectionner à la main
    counts() %>%
      filter(symbol %in% input$sel_gene_hm) %>%
      select(echantillons) %>%
      as.matrix()
  }
})



output$heatmap <- renderPlot({
  req(heatmap_data())
  pheatmap(
    mat = heatmap_data(),
    color = colorRampPalette(brewer.pal(9, "YlOrRd"))(255),
    cluster_rows = TRUE,
    show_rownames = FALSE,
    # annotation_col = conf_file() %>%
    #   filter(Condition %in% input$sel_cond) %>%
    #   select(-File),
    border_color = NA,
    fontsize = 10,
    scale = "row",
    fontsize_row = 10
  )
})