observe({
  updateSelectInput(
    inputId = "excl_samp",
    choices = my_values$config$Name
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
  
  PCAdata<-as.data.frame(pc$x[,1:3])
  PCAdata$condition <- rld_tr$Condition
  list("data" = PCAdata, "variance" = variance)
})

output$pca <- renderPlot({
  req(pca_data())
  my_lil_pca(pca_data())
})