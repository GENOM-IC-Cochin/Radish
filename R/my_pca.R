my_pca <- function(pca_data, theme = "Gray") {
    ggplot(pca_data$data,
           aes(x = PC1,
               y = PC2,
               col = Condition,
               label = rownames(pca_data$data))) + 
        geom_point(aes(shape = Condition, color = Condition), size = 5) +
        geom_point() +
        geom_label_repel() +
        xlab(paste0("PC1: ", round(pca_data$variance[1],1), "% variance")) +
        ylab(paste0("PC2: ", round(pca_data$variance[2],1), "% variance")) +
        coord_fixed() +
        switch(theme,
               "Gray" = theme_gray(),
               "Classic" = theme_classic(),
               "Classic with gridlines" = theme_bw())
}
