y_max <- function(donnees) {
  # Returns the maximum of the -log10padj (for vp)
  donnees %>%
    select(log2FoldChange, padj) %>%
    na.omit() %>%
    transmute(log_padj = -log10(padj)) %>%
    max() %>%
    ceiling()
}


x_max_abs <- function(donnees) {
  # Returns the maximum (absolute) of the ldc (for vp)
  min_x <- donnees %>%
    select(log2FoldChange, padj) %>%
    na.omit() %>%
    pull(log2FoldChange) %>%
    min() %>%
    floor()
  max_x <- donnees %>%
    select(log2FoldChange, padj) %>%
    na.omit() %>%
    pull(log2FoldChange) %>%
    max() %>%
    ceiling()
  # maximum value of the x axis
  max(c(abs(min_x), abs(max_x)))
}


tidy_symbols <- function(symbols) {
    # Makes symbols unique, if they are not NA
    indx <- which(!is.na(symbols))
    symbols[indx] <- make.unique(symbols[indx])
    return(symbols)
}


recalculate_rld_pca <- function(txi.rsem, drop_samp, configuration) {
  # Fonction qui recalcule la normalisation DESeq2, puis le rlog/vst pour la PCA
  # Nécéssaire si on élimine un échantillon considéré comme outlier
  dds <- DESeqDataSetFromTximport(txi.rsem, configuration, ~ 1)
  dds <- dds[, -drop_samp]
  dds <- estimateSizeFactors(dds)
  # filter out genes where there are less than 3 samples with normalized counts greater than or equal to 10.
  idx <- rowSums(DESeq2::counts(dds, normalized = TRUE) >= 10) >= 3 
  dds <- dds[idx, ]
  dds <- estimateDispersions(dds) # Pas de DESeq(), car le nombre d'ech peut alors être insuffisant
  if(ncol(dds) <= 30) {
    rlog(dds, blind = TRUE)
  } else {
    vst(dds, blind = TRUE)
  }
}

res_filter <- function(deseq_results,
                     lfc_filter = 0,
                     pval_filter = 1) {
    #Ajoute la colonne sur l'expression significative
    deseq_results %>%
        mutate(sig_expr = factor(case_when(
            log2FoldChange >= lfc_filter & padj <= pval_filter ~ "up",
            log2FoldChange <= -lfc_filter & padj <= pval_filter ~ "down",
            TRUE ~ "ns"
        ))) %>%
        mutate(sig_expr = relevel(sig_expr, "up"))
}



# For plotly plotting : 
# with_options(list(digits = 2, scipen = 5), ggplotly(my_maplot(test_res), tooltip = c("x", "y", "text")))

