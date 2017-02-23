#' @import dplyr
NULL

#' Append probeset ids when more than 1 for gene symbol
#' @param seg data.frame of cnv
#' @param column column name for patients
#' @param patient string of patient to keep
#' @param ref data.frame of genes
#' @return A data.frame of assigned cnvs
#' @export
#' @examples
#' \dontrun{
#' library(bifag)
#'
#' res <- append_multiprobe_genes(my_df)
#' }
#'

append_multiprobe_genes <- function(df) {

  df %>%
    group_by(gene_symbol) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(gene_symbol = if_else(n > 1, paste(gene_symbol, probeset_id, sep = "_"), gene_symbol)) %>%
    select(-probeset_id, -n)
}