test_that("test_tf_idf", {
  filepath <- testthat::test_path("testdata", "examplecorpus")

  # Function run without errors
  expect_no_error(
    res <- tf_idf(filepath=filepath, group="PUBLISHER", number_groups=1, term=list("preface"))
  )

  # Result has 1 row
  expect_equal(
    nrow(res), 1
  )

  # TF-IDF of preface must be 0.00000391
  # The collocate must be "recall"
  expect_gt(
    res["tf_idf"][1], 0.0000039
  )
  expect_lt(
    res["tf_idf"][1], 0.000004
  )
})

test_that("test_grab_collocates", {
  filepath <- testthat::test_path("testdata", "examplecorpus")

  # Function run without errors
  expect_no_error(
    colls <- grab_collocates(filepath=filepath, terms=list("preface"), horizon=2)
  )

  # Result has 1 row
  expect_equal(
    nrow(colls), 1
  )

  # The collocate must be "recall"
  expect_equal(
    (colls["collocate"][[1]] == "recall"), TRUE
  )
})
