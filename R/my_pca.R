my_pca <- function(pca_data,
                   theme = "Gray",
                   show_labels = TRUE,
                   color_by_cond) {
  pca_data$data$Condition %<>% as.character()
  plot_res <- ggplot(
    pca_data$data,
    aes(
      x = PC1,
      y = PC2,
      label = rownames(pca_data$data)
    )
  ) +
    geom_point(aes(col = Condition), size = 5) +
    xlab(paste0("PC1: ", round(pca_data$variance[1], 1), "% variance")) +
    ylab(paste0("PC2: ", round(pca_data$variance[2], 1), "% variance")) +
    coord_fixed() +
    switch(theme,
      "Gray" = theme_gray(),
      "Classic" = theme_classic(),
      "Classic with gridlines" = theme_bw()
    )
  if (show_labels) {
    plot_res <- plot_res +
      geom_label_repel(aes(col = Condition), show.legend = FALSE)
  }
  if (!is.null(color_by_cond)) {
    plot_res <- plot_res + scale_color_manual(values = color_by_cond)
  }
  plot_res
}
rld_pca <- function(rld, config, txi.rsem, excl_samp_names, ntop) {
  if (!is.null(excl_samp_names)) {
    drop_samp <- which((colnames(txi.rsem$counts)) %in% excl_samp_names)
    rld_tr <- recalculate_rld_pca(txi.rsem, drop_samp, config)
  } else {
    rld_tr <- rld
  }

  rv <- rowVars(assay(rld_tr))
  selected_genes <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  mat <- t(assay(rld_tr)[selected_genes, ])
  pc <- prcomp(mat)
  eig <- (pc$sdev)^2
  variance <- eig * 100 / sum(eig)

  PCAdata <- as.data.frame(pc$x)
  # Join with condition, on name, to be sure of matches between condition and sample
  PCAdata <- PCAdata %>%
    rownames_to_column(var = "Name") %>%
    inner_join(config, by = "Name") %>%
    select(-File) %>%
    column_to_rownames(var = "Name")
  list("data" = PCAdata, "variance" = variance)
}


recalculate_rld_pca <- function(txi.rsem, drop_samp, configuration) {
  # Fonction qui recalcule la normalisation DESeq2, puis le rlog/vst pour la PCA
  # Nécéssaire si on élimine un échantillon considéré comme outlier

  withProgress(message = "Recalculating...", {
    dds <- DESeqDataSetFromTximport(txi.rsem, configuration, ~1)
    dds <- dds[, -drop_samp]
    dds <- estimateSizeFactors(dds)
    # filter out genes where there are less than 3 samples with normalized counts greater than or equal to 10.
    idx <- rowSums(DESeq2::counts(dds, normalized = TRUE) >= 10) >= 3
    dds <- dds[idx, ]
    dds <- estimateDispersions(dds) # Pas de DESeq(), car le nombre d'ech peut alors être insuffisant
    if (ncol(dds) <= 30) {
      res <- rlog(dds, blind = TRUE)
    } else {
      res <- vst(dds, blind = TRUE)
    }
  })
  res
}
