context("rna-seq analysis")

test_that("number of retained target", {
  deseq2table <- data.frame(padj = c(.004, 0.1, 0.002),
                         log2FoldChange = c(1.2, 0.8, 0.8),
                         stringsAsFactors = FALSE)
  expect_equal(n_targets(deseq2table), 1L) # default pval = 0.05, logratio = 1
  expect_equal(n_targets(deseq2table, pval = 0.11), 1L) # default pval = 0.05, logratio = 1
  expect_equal(n_targets(deseq2table, logratio = 0.5), 2L) # default pval = 0.05, logratio = 1
  expect_equal(n_targets(deseq2table, pval = 0.2, logratio = 0.5), 3L) # default pval = 0.05, logratio = 1
})

test_that("top / bottom target", {
  dtable <- data.frame(symbol = c("Cnl5", "ARH", "LAR2", "Bght", "Serpina3n",
                                  "Serpinb2a", "Tnnc5", "Hla-a2", "Hlb-bs", "Etp3", "Gba2"),
                       ensembl_id = c("ENSMUSG00000045042",
                                      "ENSMUSG00000026164", "ENSMUSG00000066516", "ENSMUSG00000000582",
                                      "ENSMUSG00000021691", "ENSMUSG00000044634", "ENSMUSG00000096898",
                                      "ENSMUSG00000064917", "ENSMUSG00000052365", "ENSMUSG00000027724",
                                      "ENSMUSG00000030068"),
                       log2FoldChange = c(3.07325422299876, 3.03326251343897,
                                          2.86897201017694, 2.75353255557325, 2.67988667716247, -0.849319134752644,
                                          -0.85729946979555, -0.858059164589434, -0.901387500414467, -0.964155226120386,
                                          -1.57574549335941),
                       pvalue = c(6.61756558332232e-65, 5.91418301664788e-101,
                                  1.74467317135018e-142, 3.19721413822122e-124, 3.37525574701457e-97,
                                  4.31297148042962e-21, 1.59640141454307e-14, 6.35852268616939e-14,
                                  1.28505033690383e-16, 1.01894945479088e-29, 1.28940928193284e-30
                       ),
                       padj = c(1.68086165816387e-62, 3.39068561177304e-98, 3.18260107784661e-139,
                                2.78936082163248e-121, 1.73661235434857e-94, 2.07539773923983e-19,
                                4.80981843606926e-13, 1.84645609581295e-12, 4.46894628428287e-15,
                                7.62919394023648e-28, 9.95126409664013e-29),
                       stringsAsFactors = FALSE)
  expect_equal(nrow(top_bottom(dtable, n = 4)), 8L)
  expect_equal(nrow(top_bottom(dtable, n = 2)), 4L)
  res <- top_bottom(dtable, n = 2)
  expect_equal(res[1, "symbol"], "Cnl5")
  expect_equal(res[nrow(res), "symbol"], "Gba2")
})
