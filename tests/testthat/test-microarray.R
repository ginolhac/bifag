context("microarray analysis")

test_that("append probeset ids", {
  toptable <- data.frame(gene_symbol = c("Myo", "Zpt", "Myo"),
                         probeset_id = c("1101", "1102", "1103"),
                         stringsAsFactors = FALSE)
  res <- append_multiprobe_genes(toptable)
  expect_equal(res$gene_symbol, c("Myo_1101", "Zpt", "Myo_1103"))
})

test_that("append probeset ids, col names", {
  toptable2 <- data.frame(my_gene = c("Myo", "Zpt", "Myo"),
                         my_probe = c("1101", "1102", "1103"),
                         stringsAsFactors = FALSE)
  res2 <- append_multiprobe_genes(toptable2, gene = "my_gene", probe = "my_probe")
  expect_equal(res2$my_gene, c("Myo_1101", "Zpt", "Myo_1103"))
})