y_max <- function(donnees) {
   donnees %>%
       na.omit() %>%
       transmute(log_padj = -log10(padj)) %>%
       max() %>%
       ceiling()
}

x_max_abs <- function(donnees) {
    min_x <- donnees %>%
        pull(log2FoldChange) %>%
        min() %>%
        floor()
    max_x <- donnees %>%
        pull(log2FoldChange) %>%
        max() %>%
        ceiling()
    # maximum value of the x axis
    max(c(abs(min_x), abs(max_x)))
}

find_symb_col <- function(input_table) {
    cols <- colnames(input_table)
    col_symb_test <- str_detect(cols, "symbol")
        shinyFeedback::feedbackDanger(
            "inp_res_table",
            !any(col_symb_test),
            "The table does not contain a symbol (gene name) column"
        )
        req(any(col_symb_test))
        col_symbol <- which(col_symb_test)
        colnames(input_table)[col_symbol] <- "symbol"
        return(input_table)
}

tidy_symbols <- function(symbols) {
    # Makes symbols unique, if they are not NA
    indx <- which(!is.na(symbols))
    symbols[indx] <- make.unique(symbols[indx])
    return(symbols)
}

res_volc <- function(deseq_results) {
    #Ajoute la colonne sur l'expression significative
    deseq_results %>%
        mutate(sig_expr = factor(case_when(
            log2FoldChange >= 1 & padj <= 0.05 ~ "up",
            log2FoldChange <= -1 & padj <= 0.05 ~ "down",
            TRUE ~ "ns"
        ))) %>%
        mutate(sig_expr = relevel(sig_expr, "up"))
}


volcano_plot <- function(plot_data,
                         titre,
                         colors = c("up" = "#fe7f00", "down" = "#007ffe"),
                         legends,
                         axis_max,
                         ratio,
                         theme,
                         selected_genes = NULL,
                         label_size
                         ) {
    # Choice of colors/transparency for up/down
    cols <- c(colors, "ns" = "black")
    alphas <- c("up" = 1, "down" = 1, "ns" = 0.3)

    tmp <- plot_data %>%
        ggplot(aes(
            x = log2FoldChange,
            y = -log10(padj),
            alpha = sig_expr,
            fill = sig_expr
        )) +
        geom_point(
            color = "black",
            na.rm = TRUE,
            shape = 21,
            stroke = 0.1
        ) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
        geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
        scale_fill_manual(
            values = cols,
            labels = legends 
        ) +
        scale_alpha_manual(values = alphas, guide = "none") +
        labs(
            title = titre,
            x = "Log2 Fold Change",
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
               "Classic with gridlines" = theme_bw())
    
    tmp <- tmp + theme(plot.title = element_text(face = "bold",
                                                 size = 15,
                                                 hjust = 0.5),
                       aspect.ratio = ratio)
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
        genes_to_highlight <- which((plot_data$symbol %in% selected_genes |
                                         plot_data$Row.names %in% selected_genes) &
                                    (plot_data$sig_expr != "ns"))
        tmp <- tmp + geom_label_repel(
            data = plot_data[genes_to_highlight, ],
            size = label_size,
            aes(label = coalesce(symbol, Row.names)),
            color = "black",
            fill = "white",
            min.segment.length = 0,
            show.legend = FALSE
        )
    }
    tmp
}

my_lil_pca <- function(pca_data, theme = "Gray") {
    ggplot(pca_data$data,
           aes(x = PC1,
               y = PC2,
               col = condition,
               label = rownames(pca_data$data))) + 
        geom_point(aes(shape = condition, color = condition), size = 5) +
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


