test_that("load_soi works for state", {
  test1 <-
    load_soi(path = "/Volumes/davidlucey/aDL/data/irs_zip_soi", state = "CT")
  expect_equal(length(unique(test1$year)), 14)
  expect_s3_class(test1, "data.table")
})
