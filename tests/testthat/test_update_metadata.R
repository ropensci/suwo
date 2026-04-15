test_that("update query_wikiaves", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_wikiaves(species = 'Glaucis dohrnii', format = "sound")

  skip_if(is.null(df1))

  # remove the key with more observations
  sub_df <-
    df1[df1$key != names(which.max(table(df1$key))), ]

  up_df <- update_metadata(metadata = sub_df)

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})


test_that("update query_inaturalist", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_inaturalist(species = "Helicobacter pylori", format = "image")

  skip_if(is.null(df1))

  # remove the key with more observations
  sub_df <-
    df1[df1$key != names(which.max(table(df1$key))), ]

  up_df <- update_metadata(metadata = sub_df)

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})


test_that("update query_gbif", {
  skip_on_cran()
  skip_if_offline()
  df1 <- query_gbif(species = 'Glaucis dohrnii', format = "sound")

  skip_if(is.null(df1))

  # remove the key with more observations
  sub_df <-
    df1[df1$key != names(which.max(table(df1$key))), ]

  up_df <- update_metadata(metadata = sub_df)

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})
options(verbose = TRUE)

test_that("update gbif", {
  skip_on_cran()
  skip_if_offline()

  df1 <- query_gbif(
    species = 'Phaethornis anthophilus',
    all_data = FALSE,
    format = "image"
  )

  skip_if(is.null(df1))

  # remove last 3 rows to test update_metadata
  sub_df <- df1[df1$key != df1$key[1], ]

  up_df <- update_metadata(metadata = sub_df)

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})

test_that("update query_macaulay", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(
    species = 'Glaucis dohrnii',
    format = "sound",
    path = tempdir()
  )

  # remove the key with more observations
  sub_df <-
    df1[df1$key != names(which.max(table(df1$key))), ]

  up_df <- update_metadata(metadata = sub_df, path = tempdir())

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})


test_that("update query_macaulay with paging", {
  skip_if_not(interactive())
  skip_on_cran()
  skip_if_offline()

  df1 <- query_macaulay(
    species = 'Glaucis dohrnii',
    format = "sound",
    path = tempdir(),
    dates = c(1979, 2022, 2026)
  )

  # remove the key with more observations
  sub_df <-
    df1[df1$key != names(which.max(table(df1$key))), ]

  up_df <- update_metadata(
    metadata = sub_df,
    path = tempdir(),
    dates = c(1979, 2022, 2026)
  )

  skip_if(is.null(up_df))

  expect_true(nrow(up_df) == nrow(df1))
})
