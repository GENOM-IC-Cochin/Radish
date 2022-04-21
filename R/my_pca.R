my_pca <- function(pca_data,
                   theme = "Gray",
                   show_labels = TRUE,
                   color_by_cond) {
  plot_res <- ggplot(pca_data$data,
                     aes(x = PC1,
                         y = PC2,
                         label = rownames(pca_data$data))) + 
    geom_point(aes(shape = Condition, col = Condition), size = 5) +
    guides(shape = "none") +
    xlab(paste0("PC1: ", round(pca_data$variance[1],1), "% variance")) +
    ylab(paste0("PC2: ", round(pca_data$variance[2],1), "% variance")) +
    coord_fixed() +
    switch(theme,
           "Gray" = theme_gray(),
           "Classic" = theme_classic(),
           "Classic with gridlines" = theme_bw())
  if(show_labels)
    plot_res  <- plot_res + geom_label_repel(aes(col = Condition)) 
  if(!is.null(color_by_cond)){
    plot_res <- plot_res + scale_color_manual(values = color_by_cond)
  }
  plot_res
}
