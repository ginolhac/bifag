#' @import dplyr
NULL

#' Output number of retained targets according to p-value and logFC threshold
#'
#' for DESeq2 results input tables
#' @param .data data.frame or tibble
#' @param pval adjusted p-value threshold
#' @param logratio log ratio threhold
#' @return number of retained targets
#' @export

n_targets <- function(.data, pval = 0.05, logratio = 1) {
  .data %>%
    as.data.frame() %>%
    dplyr::filter(padj < pval, abs(log2FoldChange) >= logratio) %>%
    nrow()
}

#' Output top & bottom targets after sorting by logFC
#'
#' for DESeq2 results input tables
#' @param .data data.frame or tibble
#' @param n desired number of top / bottom targets
#' @return tibble with 2 x n targets
#' @export
top_bottom <- function(.data, n = 20, id = "ensembl_id", symbol = "symbol") {
  nn <- enquo(n)
  iid <- enquo(id)
  ssymbol <- enquo(symbol)
  .data %>%
  arrange(desc(log2FoldChange)) %>%
    dplyr::select(!!ssymbol, !!iid:log2FoldChange, pvalue, padj) %>%
    # use rank to deal with eventual ties
    dplyr::filter(rank(log2FoldChange, ties.method = "first") %in% c(1:!!nn, (n() - (!!nn - 1)):n()))
}