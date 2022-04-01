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


rld_pca <- function(rld, config, txi.rsem, excl_samp_names, ntop) {
  if(!is.null(excl_samp_names)) {
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
  variance <- eig*100/sum(eig)
  
  PCAdata<-as.data.frame(pc$x)
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
  
  withProgress(message = "Recalculating...",{
    dds <- DESeqDataSetFromTximport(txi.rsem, configuration, ~ 1)
    dds <- dds[, -drop_samp]
    dds <- estimateSizeFactors(dds)
    # filter out genes where there are less than 3 samples with normalized counts greater than or equal to 10.
    idx <- rowSums(DESeq2::counts(dds, normalized = TRUE) >= 10) >= 3 
    dds <- dds[idx, ]
    dds <- estimateDispersions(dds) # Pas de DESeq(), car le nombre d'ech peut alors être insuffisant
    if(ncol(dds) <= 30) {
      res <- rlog(dds, blind = TRUE)
    } else {
      res <- vst(dds, blind = TRUE)
    }
  })
  res
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

