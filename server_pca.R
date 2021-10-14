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
    drop_samp <- which(my_values$rld$Name %in% input$excl_samp)
    rld_tr <- my_values$rld[, -drop_samp]
  } else {
  rld_tr <- my_values$rld
  }

  rv <- rowVars(assay(rld_tr))
  selected_genes <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  mat <- t(assay(rld_tr)[selected_genes, ])
  pc <- prcomp(mat)
  eig <- (pc$sdev)^2
  variance <- eig*100/sum(eig)
  
  PCAdata<-as.data.frame(pc$x)
  PCAdata$condition <- rld_tr$Condition
  list("data" = PCAdata, "variance" = variance)
})

output$pca <- renderPlot({
  req(pca_data())
  my_lil_pca(pca_data(), theme = input$theme_pca)
})

output$scree <- renderPlot({
  req(pca_data())
  browser()
  data.frame(variance_exp = pca_data()$variance) %>%
    rownames_to_column() %>%
    ggplot(aes(x = rowname, y = variance_exp)) +
    geom_bar(stat = "identity")
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
