condition_possibles <- reactive({
  req(my_values$config, my_values$all_results)
  condition <- my_values$config$Condition %>% unique() 
  if(input$top_gene) {
    cond_act <- input$contrast_act %>%
      strsplit(., split = "_vs_") %>%
      unlist()
    condition[condition %in% cond_act]
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
                  "from the current contrast", input$contrast_act)
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
  req(my_values$counts)
  updateSelectizeInput(
    inputId = "sel_gene_hm",
    choices = as.vector(my_values$counts$symbol),
    server = TRUE
  )
})


heatmap_data <- eventReactive(input$draw_hm, {
  req(input$sel_cond,
      my_values$config,
      my_values$counts,
      my_values$rld,
      res(),
      input$sel_cond)
  # sélection du nom des échantillons
  echantillons <- my_values$config %>%
    filter(Condition %in% input$sel_cond) %>%
    pull(Name)
  if(input$top_gene) {
    req(input$nb_top_gene)
    # Si l'on veut que les plus différentiellement exprimés
    res() %>%
      select(Row.names, padj, log2FoldChange) %>%
      filter(log2FoldChange > 1 | log2FoldChange < -1 & padj < 0.05) %>%
      slice_min(order_by = padj, n = input$nb_top_gene) %>%
      inner_join(my_values$rld, by = "Row.names", copy = TRUE) %>%
      select(all_of(echantillons)) %>%
      as.matrix()
  } else {
    # Si l'on veut sélectionner à la main
    # Utilisation de counts juste pour les symboles
    my_values$counts %>%
      select(Row.names, symbol) %>%
      filter(symbol %in% input$sel_gene_hm) %>%
      inner_join(my_values$rld, by = "Row.names", copy = TRUE) %>%
      column_to_rownames(var = "symbol") %>%
      select(all_of(echantillons)) %>%
      as.matrix()
  }
})


# Met en correspondance les conditions choisies et les échantillons
annotation_col <- eventReactive({
  my_values$config
  input$draw_hm
},{
  my_values$config %>%
      filter(Condition %in% input$sel_cond) %>%
      select(-File) %>%
      column_to_rownames(var = "Name")
})

# A partir des annotations, leur affecte une couleur
annotation_colors <- eventReactive({
  my_values$config
  annotation_col()
}, {
  res <- vector(mode = "list", length = ncol(annotation_col()))
  names(res) <- colnames(annotation_col())
  for (name in names(res)) {
    quels_cond <- unique(annotation_col()[, name]) 
    res[[name]] <- setNames(condition_colors[1:length(quels_cond)], quels_cond)
  }
  res
})

heatmap_plot <- reactive({
  req(heatmap_data(),
      annotation_col(),
      annotation_colors())
  pheatmap( # C'est le traducteur de ComplexHeatmap
    mat = heatmap_data(),
    color = brewer.pal(9, input$palette_hm),
    cluster_rows = TRUE,
    show_rownames = input$show_names,
    annotation_col = annotation_col(),
    annotation_colors = annotation_colors(),
    border_color = NA,
    fontsize = 10,
    scale = "row",
    fontsize_row = 10
  )
})

output$heatmap <- renderPlot({
  heatmap_plot()
})


output$down_hm <- downloadHandler(
  filename = function() {
    paste0("heatmap", input$heatmap_format)
  },
  content = function(file) {
    if(input$heatmap_format == "png") {
      agg_png(file, width = 7, height = 7, units = "in", res = 600)
    } else if (input$heatmap_format == "pdf") {
      pdf(file)
    } else if (input$heatmap_format == "svg") {
      svglite(file, width = 7, height = 7)
    }
    draw(heatmap_plot())
    dev.off()
  }
)