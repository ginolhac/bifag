## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/STAT545-UBC/Discussion/issues/451
if (getRversion() >= "2.15.1")  utils::globalVariables(c("probeset_id", "gene_symbol"))