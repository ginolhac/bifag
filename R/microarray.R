#' @import dplyr
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang has_name
#' @importFrom rlang :=
NULL

#' Summarised gene symbols by probeset
#'
#' Append probeset ids when more than 1 for gene symbol
#' the returned data frame will have unique gene symbols present only once
#' @param df data.frame or tibble
#' @param gene unquotted column name for gene symbols
#' @param probe unquotted column name for probeset_id
#' @param ... optional groups on top of gene symbol to avoid appending
#' @return A data.frame of summarised gene symbols
#' @export
#' @examples
#' \dontrun{
#' library(bifag)
#'
#' res <- append_multiprobe_genes(df)
#' }
#'

append_multiprobe_genes <- function(df, gene = gene_symbol, probe = probeset_id, ...) {
  my_gene <- enquo(gene)
  my_probe <- enquo(probe)
  my_groups <- enquos(...)
  # Check whether data.frame name is supplied
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame / tibble", call. = FALSE)
  }
  if (!has_name(df, quo_name(my_gene))) {
    stop(paste(quo_name(my_gene), "must be a column in the specified dataframe"), call. = FALSE)
  }
  if (!has_name(df, quo_name(my_probe))) {
    stop(paste(quo_name(my_probe), "must be a column in the specified dataframe"), call. = FALSE)
  }

  df %>%
    group_by(!!my_gene, !!!my_groups) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(!!quo_name(my_gene) := if_else(n > 1, paste(!!my_gene, !!my_probe, sep = "_"), !!my_gene)) %>%
    select(-n)
}
