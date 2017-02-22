library(dplyr)

append_multiprobe_genes <- function(df) {

  df %>%
    group_by(gene_symbol) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(gene_symbol = if_else(n > 1, paste(gene_symbol, probeset_id, sep = "_"), gene_symbol)) %>%
    select(-probeset_id, -n)
}