options(verbose = TRUE)

test_that("search Glaucis dohrnii audio", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(species = "Glaucis dohrnii", format = "sound")
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 20)
})


test_that("search Floractus heimi audio (no audios)", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(species = 'Floractus heimi', format = "sound")

  expect_true(is.null(df1))
})

test_that("search Glaucis dohrnii photos", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(species = 'Glaucis dohrnii', format = "image")
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 29)
})

test_that("search Glaucis dohrnii photos specific data set", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(
    species = 'Glaucis dohrnii',
    format = "image",
    dataset = "b1047888-ae52-4179-9dd5-5448ea342a24"
  )
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 22)
})


test_that("no result", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(species = 'asdasdasd', format = "image")

  expect_true(is.null(df1))
})


test_that("search Aristolochia baetica photos in parallel", {
  skip_on_cran()
  skip_if_offline()

  cores <- parallel::detectCores()
  cores <- if (cores > 2) {
    2
  }
  if (!is.numeric(cores)) {
    cores <- 1
  }
  df1 <- query_gbif(
    species = 'Aristolochia baetica',
    format = "image",
    cores = cores
  )
  skip_if(is.null(df1))

  expect_true(nrow(df1) >= 2003)
})


test_that("test verbose FALSE", {
  skip_on_cran()
  skip_if_offline()

  msg <- capture.output(
    df1 <- query_gbif(
      species = 'Glaucis dohrnii',
      format = "image",
      verbose = FALSE,
      pb = FALSE
    )
  )
  skip_if(is.null(df1))

  expect_true(length(msg) == 0)
})

test_that("test all_data FALSE", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(
    species = 'Glaucis dohrnii',
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
