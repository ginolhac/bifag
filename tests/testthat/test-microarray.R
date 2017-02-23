context("microarray analysis")

test_that("append probeset ids", {
  toptable <- data.frame(gene_symbol = c("Myo", "Zpt", "Myo"),
                         probeset_id = c("1101", "1102", "1103"),
                         stringsAsFactors = FALSE)
  res <- append_multiprobe_genes(toptable)
  expect_equal(res$gene_symbol, c("Myo_1101", "Zpt", "Myo_1103"))
})
