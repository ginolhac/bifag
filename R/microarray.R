#' @import dplyr
#' @importFrom lazyeval interp
#' @importFrom stats setNames
NULL

#' Summarised gene symbols by probeset
#'
#' Append probeset ids when more than 1 for gene symbol
#' the returned data frame will have unique gene symbols present only once
#' @param df data.frame or tibble
#' @param gene column name for gene symbols
#' @param probe column name for probeset_id
#' @return A data.frame of summarised gene symbols
#' @export
#' @examples
#' \dontrun{
#' library(bifag)
#'
#' res <- append_multiprobe_genes(df)
#' }
#'

append_multiprobe_genes <- function(df, gene = "gene_symbol", probe = "probeset_id") {
  # Check whether data.frame name is supplied
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame / tibble", call. = FALSE)
  }
  if (gene %in% colnames(df) == FALSE) {
    stop(paste(gene, "must be a column in the specified dataframe"), call. = FALSE)
  }

  if (probe %in% colnames(df) == FALSE) {
    stop(paste(probe, "must be a column in the specified dataframe"), call. = FALSE)
  }
  # columns to keep in the end, drop n and probeset
  keep <- names(df)[!names(df) %in% c(probe, "n")]
  # mix od constant and variables, better use lazyeval with
  # - n is from the data frame so we quote it
  # gene comes is local variable from formals
  mut_call <- lazyeval::interp(~if_else(nb > 1, paste(gs, pbs, sep = "_"), gs),
                               nb = quote(n),
                               gs = as.name(gene),
                               pbs = as.name(probe))
  df %>%
    group_by_(gene) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    # overwrite the gene symbol column, use the interp as name and setNames to get
    # the dynamic col name right
    mutate_(.dots = setNames(list(mut_call), gene)) %>%
    select_(.dots = keep)
  # alternative for negative selection
  # select_(lazyeval::interp(~-prob, prob = as.name(probe)))
}
