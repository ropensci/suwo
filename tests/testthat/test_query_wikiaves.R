options(verbose = TRUE)

test_that("search Glaucis dohrnii sound", {
  skip_on_cran()
  skip_if_offline()

  df1 <- try(
    query_wikiaves(species = 'Glaucis dohrnii', format = "sound"),
    silent = TRUE
  )
  skip_if(is.null(df1))
  skip_if(suwo:::.is_error(df1))

  expect_true(nrow(df1) >= 30)
})

test_that("search Piranga flava sound", {
  skip_on_cran()
  skip_if_offline()

  df1 <- try(
    query_wikiaves(species = 'Piranga flava', format = "sound"),
    silent = TRUE
  )
  skip_if(is.null(df1))
  skip_if(suwo:::.is_error(df1))

  expect_true(nrow(df1) >= 140)
})

test_that("search Glaucis dohrnii photos", {
  skip_on_cran()
  skip_if_offline()

  df1 <- try(
    query_wikiaves(species = 'Glaucis dohrnii', format = "image"),
    silent = TRUE
  )

  skip_if(is.null(df1))

  skip_if(suwo:::.is_error(df1))

  expect_true(nrow(df1) >= 420)
})


test_that("no result", {
  skip_on_cran()
  skip_if_offline()

  expect_null(query_wikiaves(species = 'asdasdasd', format = "image"))
})


test_that("test verbose FALSE", {
  skip_on_cran()
  skip_if_offline()

  df1 <- try(
    testthat::capture_output(query_wikiaves(
      species = 'a3',
      format = "sound",
      verbose = FALSE,
      pb = FALSE
    )),
    silent = TRUE
  )
  skip_if(is.null(df1))
  skip_if(suwo:::.is_error(df1))

  expect_true(df1 == "")
})

test_that("test all_data FALSE", {
  skip_on_cran()
  skip_if_offline()

  df1 <- try(
    query_wikiaves(
      species = 'Glaucis dohrnii',
      format = "sound",
      all_data = FALSE
    ),
    silent = TRUE
  )

  skip_if(suwo:::.is_error(df1))
  skip_if(is.null(df1))

  expected_col_names <- suwo:::.format_query_output(only_basic_columns = TRUE)

  query_col_names <- colnames(df1)

  expect_true(
    all(query_col_names %in% expected_col_names) &
      all(expected_col_names %in% query_col_names),
    info = "Column names do not match the expected names"
  )
})
