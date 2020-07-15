context("helpers")

test_that("packages loaded", {

  skelet <- system.file("tests", "test_pkgs.Rmd", package = "bifag")
  message(skelet)
  res_all <- session_info_nodep(file = skelet) # same as , type = "all
  res_lib <- session_info_nodep(file = skelet, type = "library")
  expect_equal(nrow(res_all), 2)
  expect_equal(nrow(res_lib), 1)
  expect_equal(ncol(res_all), 4)
  expect_equal(ncol(res_lib), 4)
  expect_equal(res_all$package, c("stats", "bifag"))
  expect_equal(res_lib$package, "stats")
})


test_that("str_subset", {

  my_string <- c("library(tidyverse)", "tidyverse" , NA, "bifag::session_info_nodep(")
  res <- str_subset_inv(my_string, "\\(")
  expect_equal(res[1], "tidyverse")
  expect_equal(length(res), 2)
})

test_that("dates", {
  dtmp <- tempfile()
  # to create the tempfile
  write("abc", dtmp)
  dates <- dates_report(dtmp)
  rex <- "^\\d{4}-\\d{2}-\\d{2} \\(last change: \\d{4}-\\d{2}-\\d{2}, compiled: \\d{4}-\\d{2}-\\d{2}\\)"
  expect_true(grepl(rex, dates))
})