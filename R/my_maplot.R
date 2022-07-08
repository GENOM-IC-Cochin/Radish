my_maplot <- function(plot_data,
                      title = "",
                      colors = c("up" = "#fe7f00", "down" = "#007ffe"),
                      legends = c("up" = "up", "down" = "down", "ns" = "ns"),
                      ratio = 1,
                      selected_genes = NULL,
                      theme = "Classic",
                      y_axis_max,
                      label_size = 3.8) {
  color_points <- c(colors, "ns" = "black")
  alphas <- c("up" = 1, "down" = 1, "ns" = 0.3)
  
  plot_res <- ggplot(plot_data, aes(
    x = baseMean,
    y = log2FoldChange,
    fill = sig_expr,
    alpha = sig_expr,
    shape = outside,
    text = paste0(
      "<b>Gene Name : ",
      coalesce(symbol, Row.names),
      "</b><br>",
      "Mean Expression : ",
      signif(baseMean, 3),
      "<br>",
      "log2(FoldChange) : ",
      signif(log2FoldChange, 3)
      )
  )) +
    geom_point(
      color = "black",
      na.rm = TRUE,
      stroke = 0.1
    ) +
    scale_fill_manual(
      values = color_points,
      labels = legends
    ) +
    scale_shape_manual(
      values = c("in" = 21, "out" = 24),
      guide = "none"
    ) +
    scale_alpha_manual(values = alphas, guide = "none") +
    geom_hline(yintercept = 0) +
    scale_x_log10() +
    scale_y_continuous(
      limits = c(-y_axis_max, y_axis_max),
      oob = scales::squish
      ) +
    switch(theme,
           "Gray" = theme_gray(),
           "Classic" = theme_classic(),
           "Classic with gridlines" = theme_bw()) +
    labs(
      title = title,
      x = "Mean of normalized counts, all samples",
      y = "Log2(FoldChange)",
      fill = "Expression\nchange"
    ) +
    theme(
      plot.title = element_text(face = "bold",
                                size = 15,
                                hjust = 0.5),
      aspect.ratio = ratio
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 21)))

  if(lfc_max_abs(plot_data) != y_axis_max) { #lfc_max_abs, sic
  plot_res <- plot_res + geom_hline(
      yintercept = c(-y_axis_max, y_axis_max),
      linetype = "dotted"
      )
  }
  if (!is.null(selected_genes)) {
    # Shows gene names if there is one
    genes_to_highlight <- which((plot_data$symbol %in% selected_genes |
                                   plot_data$Row.names %in% selected_genes))
    plot_res <- plot_res + ggrepel::geom_label_repel(
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
  plot_res
}
