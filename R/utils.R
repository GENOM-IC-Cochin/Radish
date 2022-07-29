log_padj_max <- function(donnees) {
  # Returns the maximum of the -log10padj
  donnees %>%
    select(log2FoldChange, padj) %>%
    na.omit() %>%
    transmute(log_padj = -log10(padj)) %>%
    max() %>%
    ceiling()
}


lfc_max_abs <- function(donnees) {
  # Returns the maximum (absolute) of the lfc
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
  # Ajoute la colonne sur l'expression significative
  res <- deseq_results %>%
    mutate(sig_expr = factor(case_when(
      log2FoldChange >= lfc_filter & padj <= pval_filter ~ "up",
      log2FoldChange <= -lfc_filter & padj <= pval_filter ~ "down",
      TRUE ~ "ns"
    )))
  if ("up" %in% res$sig_expr) {
    res %>%
      mutate(sig_expr = relevel(sig_expr, "up"))
  }
  res
}



contr_str <- function(contrastes, contrast_act, ...) {
  paste(contrastes[strtoi(contrast_act), 2],
        contrastes[strtoi(contrast_act), 3], ...)
}


recalc_pca <- tagList(
  waiter::spin_folding_cube(),
  h3("Recomputing PCA...")
)

# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

max_exclusive_int_size <- function(upset_data) {
# Gets the max exclusive intersection size in the data
# For the uppper limit of the sliderInput
  upset_data %<>% select(where(is.logical))
  coln_data <- colnames(upset_data)
  # possible columns combinations (2 by 2)
  col_comb_list <- combn(coln_data, 2, simplify = FALSE)
  # Add also the columns on their own
  col_comb_list <- c(col_comb_list, coln_data %>% as.list)
  # The rest of columns
  anti_comb_list <- purrr::map(col_comb_list, ~ setdiff(coln_data, .))
  res <- rep(NA, length.out = length(col_comb_list))

  for (i in seq_along(col_comb_list)) {
    if (identical(anti_comb_list[[i]], character(0))) {
      except_set <- character(0)
    } else {
      # apply(data, 1, any) -> select rows where any cell is TRUE
      except_set <- upset_data[apply(upset_data %>% select(all_of(anti_comb_list[[i]])), 1, any), ] %>%
        rownames()
    }
    incl_intersect <- upset_data[apply(upset_data %>% select(all_of(col_comb_list[[i]])), 1, all), ] %>%
      rownames()
    res[i] <- setdiff(incl_intersect, except_set) %>% length
  }
  return(max(res))
}
