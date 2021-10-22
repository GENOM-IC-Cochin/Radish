observe({
  updateSelectizeInput(
    inputId = "excl_samp",
    choices = my_values$config$Name,
    # To forbid PCA plots with two samples
    options = list(maxItems = length(my_values$config$Name) - 3)
  )
})

pca_data <- reactive({
  ntop <- 500
  req(my_values$rld)
  
  # Exclude samples
  if(!is.null(input$excl_samp)) {
    drop_samp <- which((my_values$rld %>% colnames) %in% input$excl_samp)
    rld_tr <- my_values$rld[, -drop_samp]
  } else {
  rld_tr <- my_values$rld
  }

  rv <- matrixStats::rowVars(rld_tr)
  selected_genes <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  mat <- t(rld_tr[selected_genes, ])
  pc <- prcomp(mat)
  eig <- (pc$sdev)^2
  variance <- eig*100/sum(eig)
  
  PCAdata<-as.data.frame(pc$x)
  # Join with condition, on name, to be sure of matches
  PCAdata <- PCAdata %>%
    rownames_to_column(var = "Name") %>%
    inner_join(configuration, by = "Name", copy = TRUE) %>%
    select(-File) %>%
    column_to_rownames(var = "Name")
  list("data" = PCAdata, "variance" = variance)
})

output$pca <- renderPlot({
  req(pca_data())
  my_lil_pca(pca_data(), theme = input$theme_pca)
})

output$scree <- renderPlot({
  req(pca_data())
  data.frame(variance_exp = pca_data()$variance,
             dimension = as.factor(1:length(pca_data()$variance))) %>%
    ggplot(aes(x = dimension, y = variance_exp)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Dimensions",
         y = "Percentage of variance") +
    geom_text(aes(label = signif(variance_exp, 3)), vjust = 1.6, colour = "white") +
    theme_bw()
})

output$down_pca <- downloadHandler(
  filename = function() {
    paste0("pca.", input$pca_format)
  },
  content = function(file) {
    ggsave(file, plot = my_lil_pca(req(pca_data()), theme = input$theme_pca),
           device = req(input$pca_format),
           dpi = 600)
  }
)
