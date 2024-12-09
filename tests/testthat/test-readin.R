test_that("getbook() works as expected", {
  test_data_path <- testthat::test_path("testdata", "examplecorpus")
  # Test that the file reads in all 7 .book files stored in inst/extdata
  expect_equal(
    length(getbook(filepath = test_data_path)), 7
  )
})

test_that("getbook() returns character data", {
  test_data_path <- testthat::test_path("testdata", "examplecorpus")
  # Test that there are multiple columns corresponding to chara data
  expect_equal(
    ncol(getbook(filepath = test_data_path)[[1]]), 8
  )
})
