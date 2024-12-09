test_that("grab sentiment returns a list of dfs", {
  test_data_path <- testthat::test_path("testdata", "examplecorpus")
  parsedbooks <- getbook(test_data_path)

  expect_equal(length(grab_sentiment(parsedbooks, colname = "Modifiers")), length(parsedbooks))
})

test_that("plot sentiment returns a ggplot", {
  test_data_path <- testthat::test_path("testdata", "examplecorpus")
  parsedbooks <- getbook(test_data_path)
  sentiment_list <- grab_sentiment(parsedbooks, colname = "Modifiers")

  expect_equal(is.ggplot(plot_sentiment(
    sentiment_list,
    measure = "sd", charalevel = 7
  )), TRUE)
})
