condition_possibles <- reactive({
  # Conditions from which choice is possible
    req(
        config(),
        all_results()
    )
  condition <- config()$Condition %>% unique()
  if(input$top_gene) {
    cond_act <- input$contrast_act %>%
      strsplit(., split = "_vs_") %>%
      unlist()
    condition[condition %in% cond_act]
  } else {
    condition
  }
})


observeEvent(input$contrast_act, {
  # Checkbox to switch between the two possible heatmaps UI
  updateCheckboxInput(
    inputId = "top_gene",
    label = paste("Top differentially expressed genes",
                  "from contrast", input$contrast_act)
  )
})


observeEvent(input$top_gene,
# Switch between top gene heatmap UI and selected genes heatmap UI
             updateTabsetPanel(
               inputId = "settings",
               selected = ifelse(input$top_gene,
                                 "diff",
                                 "all")
             )
)

observeEvent(condition_possibles(),
  updateCheckboxGroupInput(
    inputId = "sel_cond",
    choices = condition_possibles(),
    # Selected, tout, car si ce Checkbox... est pas affiché (top genes)
    # il doit alors contenir les deux conditions en contraste
    selected = condition_possibles(),
  )
)

observeEvent(counts(),
  # Updates the max number of top genes based on the total number of genes
  updateNumericInput(
    inputId = "nb_top_gene",
    max = nrow(counts())
  )
)

observeEvent({
  counts()
  sel_genes_names()
}, {
  updateSelectizeInput(
    inputId = "sel_gene_hm_nm",
    choices = as.vector(counts()$symbol),
    server = TRUE,
    selected = sel_genes_names()
  )
})

observeEvent({
  counts()
  sel_genes_table()
  }, {
  updateSelectizeInput(
    inputId = "sel_gene_hm_id",
    choices = as.vector(counts()$Row.names),
    server = TRUE,
    selected = sel_genes_table() %>%
      filter(is.na(symbol)) %>%
      pull(Row.names)
  )
})

heatmap_data <- eventReactive(input$draw_hm, {
  req(input$sel_cond,
      config(),
      counts(),
      res(),
      input$sel_cond)
  # sélection du nom des échantillons
  # Basé sur les conditions selectionnees
  # Ou par defaut dans le cas top gene
  echantillons <- config() %>%
    filter(Condition %in% input$sel_cond) %>%
    pull(Name)
  
  if(input$top_gene) {
    req(input$nb_top_gene)
    # Si l'on veut que les plus différentiellement exprimés
    res() %>%
      filter(log2FoldChange > 1 | log2FoldChange < -1,
             padj < 0.05) %>%
      slice_min(order_by = padj, n = input$nb_top_gene) %>%
      select(all_of(echantillons)) %>%
      as.matrix()
  } else {
    # Si l'on veut sélectionner à la main
    counts() %>%
      filter(symbol %in% input$sel_gene_hm_nm |
               Row.names %in% input$sel_gene_hm_id) %>%
      mutate(name = coalesce(symbol, Row.names)) %>%
      column_to_rownames(var = "name") %>%
      select(all_of(echantillons)) %>%
      as.matrix()
  }
})


# Met en correspondance les conditions choisies et les échantillons
# (Pour les couleurs de la heatmap)
# retour : un df avec la correspondance désirée
annotation_col <- eventReactive({
  config()
  input$draw_hm
  input$sel_cond
},{
  config() %>%
      filter(Condition %in% input$sel_cond) %>%
      select(-File) %>%
      column_to_rownames(var = "Name")
})

# A partir des annotations, leur affecte une couleur
# retour : une liste avec la correspondance selon le format de pheatmap
# pour le paramètre annotation_colors
annotation_colors <- eventReactive({
  config()
  annotation_col()
}, {
  # Au cas où il y ait un jour plusieurs catégories (malade, traitement ...)
  # Aujourd'hui de longueur 1
  ret <- vector(mode = "list", length = ncol(annotation_col()))
  # on y met le nom des échantillons
  names(ret) <- colnames(annotation_col())
  for (name in names(ret)) {
    # quelle condition correspond à ces ech
    quels_cond <- unique(annotation_col()[, name]) 
    # condition_colors : variable globale
    ret[[name]] <- setNames(condition_colors[1:length(quels_cond)], quels_cond)
  }
  ret
})

heatmap_plot <- eventReactive(input$draw_hm, {
  req(heatmap_data(),
      annotation_col(),
      annotation_colors())
      
  pheatmap( # C'est le traducteur de ComplexHeatmap
    name = "z-score",
    mat = heatmap_data(),
    color = rev(brewer.pal(9, input$palette_hm)),
    cluster_rows = TRUE,
    # No row names if top genes
    show_rownames = input$show_names & !input$top_gene,
    annotation_col = annotation_col(),
    annotation_colors = annotation_colors(),
    border_color = NA,
    fontsize = 10,
    fontsize_row = input$fontsize_hm,
    scale = "row",
  )
})

output$heatmap <- renderPlot({
  heatmap_plot()
})


output$down_hm <- downloadHandler(
  filename = function() {
    paste0("heatmap.", input$heatmap_format)
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
