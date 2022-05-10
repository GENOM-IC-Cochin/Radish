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


res_filter <- function(deseq_results,
                       lfc_filter = 0,
                       pval_filter = 1) {
  #Ajoute la colonne sur l'expression significative
  res <- deseq_results %>%
    mutate(sig_expr = factor(case_when(
      log2FoldChange >= lfc_filter & padj <= pval_filter ~ "up",
      log2FoldChange <= -lfc_filter & padj <= pval_filter ~ "down",
      TRUE ~ "ns"
    ))) 
  if("up" %in% res$sig_expr) {
    res %>%
      mutate(sig_expr = relevel(sig_expr, "up"))
  }
  res
}


palette_hm <- function(color_name) {
  if(color_name %in% brew_vec) {
    colorRampPalette(rev(brewer.pal(n = 9, name = color_name)))(256)
  } else {
    c(viridis::viridis(256, option = vir_vec[color_name]), use.names = FALSE)
  }
}

contr_str <- function(contrastes, contrast_act, ...) {
  paste(contrastes[strtoi(contrast_act), 2], contrastes[strtoi(contrast_act), 3], ...)
}
