test_that("search Helicobacter pylori sound", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = "Helicobacter pylori", format = "image")

  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 1)
})


test_that("search Spatula discors sound (no sounds)", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(
    species = 'Spatula discors',
    format = "sound",
    identified = TRUE,
    verifiable = TRUE
  )

  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 20)
})

test_that("search Glaucis dohrnii photos", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = 'Glaucis dohrnii', format = "image")

  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 10)
})

test_that("no result", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = 'asdasdasd', format = "image")

  expect_true(is.null(df1))
})


test_that("search in parallel", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(
    species = 'bolitoglossa striatula',
    format = "image",
    cores = 2
  )
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 52)
})


options(verbose = TRUE)

test_that("test verbose FALSE", {
  skip_on_cran()
  skip_if_offline()

  df1 <- capture_output(query_inaturalist(
    species = 'Glaucis dohrnii',
    format = "sound",
    verbose = FALSE
  ))

  skip_if(is.null(df1))

  expect_true(df1 == "")
})

test_that("test all_data FALSE", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(
    species = 'bolitoglossa striatula',
    format = "image",
    all_data = FALSE
  )

  skip_if(is.null(df1))

  expected_col_names <- .format_query_output(only_basic_columns = TRUE)

  query_col_names <- colnames(df1)
  expect_true(
    all(expected_col_names %in% query_col_names) &&
      all(query_col_names %in% expected_col_names),
    info = "Column names do not match the expected names"
  )
})
