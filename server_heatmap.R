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


heatmap_data <- eventReactive({
  input$draw_hm
  res()
  counts()
  }, {
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
      select(all_of(echantillons)) %>%
      as.matrix()
  } else {
    # Si l'on veut sélectionner à la main
    counts() %>%
      filter(symbol %in% input$sel_gene_hm) %>%
      column_to_rownames(var = "symbol") %>%
      select(all_of(echantillons)) %>%
      as.matrix()
  }
})


annotation_col <- eventReactive({
  input$draw_hm
  conf_file()
  },{
  conf_file() %>%
      filter(Condition %in% input$sel_cond) %>%
      select(-File) %>%
      column_to_rownames(var = "Name")
})

output$heatmap <- renderPlot({
  req(heatmap_data())
  pheatmap(
    mat = heatmap_data(),
    color = colorRampPalette(brewer.pal(9, input$palette_hm))(255),
    cluster_rows = TRUE,
    show_rownames = input$show_names,
    annotation_col = annotation_col(),
    border_color = NA,
    fontsize = 10,
    scale = "row",
    fontsize_row = 10
  )
})


output$down_hm <- downloadHandler(
  filename = function() {
    paste0("heatmap.png")
  },
  content = function(file) {
    if(input$heatmap_format == "png") {
      agg_png(file, width = 7, height = 7, units = "in", res = 600)
    } else if (input$heatmap_format == "pdf") {
      pdf(file)
    } else if (input$heatmap_format == "svg") {
      svglite(file, width = 7, height = 7)
    }
      pheatmap(
        mat = heatmap_data(),
        color = colorRampPalette(brewer.pal(9, input$palette_hm))(255),
        cluster_rows = TRUE,
        show_rownames = input$show_names,
        annotation_col = annotation_col(),
        border_color = NA,
        fontsize = 10,
        scale = "row",
        fontsize_row = 10
      )
      dev.off()
  }
)