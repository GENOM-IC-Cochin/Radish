my_pca <- function(pca_data,
                   theme = "Gray",
                   show_labels = TRUE,
                   color_by_level,
                   color_by = "Condition",
                   shape_by = "none") {
  pca_data$data %<>% mutate(across(where(is.factor), as.character))
  plot_res <- ggplot(
    pca_data$data,
    aes(
      x = PC1,
      y = PC2,
      label = rownames(pca_data$data)
    )
  ) +
    ylab(paste0("PC2: ", round(pca_data$variance[2], 1), "% variance")) +
    xlab(paste0("PC1: ", round(pca_data$variance[1], 1), "% variance")) +
    coord_fixed() +
    switch(theme,
           "Gray" = theme_gray(),
           "Classic" = theme_classic(),
           "Classic with gridlines" = theme_bw()
           )
  if(shape_by != "none") {
    plot_res <- plot_res +
      geom_point(aes_string(color = color_by, shape = shape_by), size = 5)
  } else {
    plot_res <- plot_res +
      geom_point(aes_string(color = color_by), size = 5)
  }
  if (show_labels) {
    plot_res <- plot_res +
      ggrepel::geom_label_repel(aes_string(color = color_by), show.legend = FALSE)
  }
plot_res <- plot_res + scale_color_manual(values = color_by_level)
plot_res

}


rld_pca <- function(rld, config, txi.rsem, excl_samp_names, ntop) {
  if (!is.null(excl_samp_names)) {
    drop_samp <- which((colnames(txi.rsem$counts)) %in% excl_samp_names)
    rld_tr <- recalculate_rld_pca(txi.rsem, drop_samp, config)
  } else {
    rld_tr <- rld
  }

  rv <- matrixStats::rowVars(assay(rld_tr))
  selected_genes <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  mat <- t(assay(rld_tr)[selected_genes, ])
  pc <- prcomp(mat)
  eig <- (pc$sdev)^2
  variance <- eig * 100 / sum(eig)

  PCAdata <- as.data.frame(pc$x)
                                        # Join with condition, on name, to be sure of matches between condition and sample
  PCAdata <- PCAdata %>%
    tibble::rownames_to_column(var = "Name") %>%
    inner_join(config, by = "Name") %>%
    select(-File) %>%
    tibble::column_to_rownames(var = "Name")
  list("data" = PCAdata, "variance" = variance)
}


recalculate_rld_pca <- function(txi.rsem, drop_samp, configuration) {
                                        # Fonction qui recalcule la normalisation DESeq2, puis le rlog/vst pour la PCA
                                        # Nécéssaire si on élimine un échantillon considéré comme outlier

  waiter::waiter_show(html = recalc_pca, color = "#07856E")
  dds <- DESeq2::DESeqDataSetFromTximport(txi.rsem, configuration, ~1)
  dds <- dds[, -drop_samp]
  dds <- DESeq2::estimateSizeFactors(dds)
                                        # filter out genes where there are less than 3 samples with normalized counts greater than or equal to 10.
  idx <- rowSums(DESeq2::counts(dds, normalized = TRUE) >= 10) >= 3
  dds <- dds[idx, ]
  dds <- DESeq2::estimateDispersions(dds) # Pas de DESeq(), car le nombre d'ech peut alors être insuffisant
  if (ncol(dds) <= 30) {
    res <- DESeq2::rlog(dds, blind = TRUE)
  } else {
    res <- DESeq2::vst(dds, blind = TRUE)
  }
  waiter::waiter_hide()
  res
}
