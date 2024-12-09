# test_that("retrieving rsdb data works", {
#
#   test_data_path <- testthat::test_path("testdata", "examplecorpus")
#   parsedbooks <- getbook(filepath = file.path("inst", "extdata", package = "examplecorpus"))
#   text <- parsedbooks[[1]]
#   # Dataframe doesn't contain any racial slurs --> should return text output
#   expect_equal(
#     is.character(grab_racialmods(text, db = "rsdb")), TRUE
#   )
#
# })
