context("DPD")

test_that("We error trying to load the DPD without a cache", {
  data_dir <- tempfile("dpd")
  expect_error(dpd_load(data_dir, download = F))
})

test_that("We can download the DPD and load it correctly", {
#  testthat::skip_on_cran()

  data_dir <- tempfile("dpd")
  dpd <- dpd_load(data_dir, download = T)

  expect_gt(length(list.files(data_dir)), 45)
  expect_length(dir(data_dir, pattern="*.txt"), 45)
  expect_is(dpd, 'list')

  tables <- c("comp","form","ingred","package","pharm","drug","route","sched","status","ther","vet")
  for (t in tables) {
    expect_true(t %in% names(dpd))
    expect_is(dpd[[t]], "data.frame")
    expect_true("DRUG_CODE" %in% names(dpd[[t]]))
  }

  unlink(data_dir, recursive=T)
})
