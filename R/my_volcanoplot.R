my_volcanoplot <- function(plot_data,
                           titre = "",
                           colors = c("up" = "#fe7f00", "down" = "#007ffe"),
                           legends = c("up" = "up", "down" = "down", "ns" = "ns"),
                           axis_max,
                           ratio = 1,
                           theme = "Classic with gridlines",
                           selected_genes = NULL,
                           label_size = 3.8,
                           lfc_cutoff = 1,
                           pval_cutoff = 0.05
) {
  # Choice of colors/transparency for up/down
  cols <- c(colors, "ns" = "black")
  alphas <- c("up" = 1, "down" = 1, "ns" = 0.3)
  tmp <- plot_data %>%
    ggplot(aes(
      x = log2FoldChange,
      y = -log10(padj),
      alpha = sig_expr,
      fill = sig_expr,
      shape = outside,
      # for ggplotly
      text = paste0("log2(FoldChange) : ",
                    signif(log2FoldChange, 2),
                    "<br>",
                    "-log10(pval) : ",
                    signif(-log10(padj), 2),
                    "<br>",
                    "Name : ",
                    coalesce(symbol, Row.names))
    )) +
    geom_point(
      na.rm = TRUE,
      color = "black",
      stroke = 0.1
    ) +
    scale_shape_manual(
      values = c("in" = 21, "out" = 24),
      guide = "none"
    ) +
    geom_hline(yintercept = -log10(pval_cutoff), linetype = "dashed") +
    geom_vline(xintercept = c(-lfc_cutoff, lfc_cutoff), linetype = "dashed") +
    scale_fill_manual(
      values = cols,
      labels = legends
    ) +
    scale_alpha_manual(values = alphas, guide = "none") +
    labs(
      title = titre,
      x = "Log2(Fold Change)",
      y = "-Log10(Adjusted p-value)",
      fill = "Expression\nChange"
    ) +
    scale_x_continuous(
      limits = c(-axis_max[1], axis_max[1]),
      oob = scales::squish
    ) +
    scale_y_continuous(limits = c(NA, axis_max[2]), oob = scales::squish) +
    switch(theme,
           "Gray" = theme_gray(),
           "Classic" = theme_classic(),
           "Classic with gridlines" = theme_bw()) +
    guides(fill = guide_legend(override.aes = list(shape = 21)))

  tmp <- tmp + theme(
    plot.title = element_text(face = "bold",
                              size = 15,
                              hjust = 0.5),
    aspect.ratio = ratio
  )
  if (axis_max[2] != y_max(plot_data)) {
    tmp <- tmp + geom_hline(yintercept = axis_max[2], linetype = "dotted")
  }
  if (axis_max[1] != x_max_abs(plot_data)) {
    tmp <- tmp + geom_vline(
      xintercept = c(-axis_max[1], axis_max[1]),
      linetype = "dotted"
    )
  }
  if (!is.null(selected_genes)) {
    # Choice of genes, do not show labels of non significant genes
    # Shows gene names if there is one
    genes_to_highlight <- which((plot_data$symbol %in% selected_genes |
                                   plot_data$Row.names %in% selected_genes))
    tmp <- tmp + geom_label_repel(
      data = plot_data[genes_to_highlight, ],
      size = label_size,
      aes(label = coalesce(symbol, Row.names)),
      color = "black",
      fill = "white",
      min.segment.length = 0,
      show.legend = FALSE,
      # There are many points
      # They count as things overlapped
      max.overlaps = Inf
    )
  }
  tmp
}
