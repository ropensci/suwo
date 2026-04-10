options(verbose = TRUE)


test_that("map default", {
  df1 <- suwo:::testing_metadata$t_rufiventris

  a <- map_locations(metadata = df1)

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")

  expect_equal(length(a$dependencies), 8)
})

test_that("map markers and cluster", {
  df1 <- suwo:::testing_metadata$t_rufiventris

  a <- map_locations(metadata = df1, type = "markers", cluster = TRUE)

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")

  expect_equal(length(a$dependencies), 11)
})


test_that("show_media FALSE", {
  df1 <- suwo:::testing_metadata$t_rufiventris

  a <- map_locations(
    metadata = df1,
    type = "markers",
    cluster = TRUE,
    show_media = FALSE
  )

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")

  expect_equal(length(a$dependencies), 11)
})

test_that("popup size", {
  df1 <- suwo:::testing_metadata$t_rufiventris

  a <- map_locations(
    metadata = df1,
    type = "markers",
    cluster = TRUE,
    popup_size = 0.4
  )

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")

  expect_equal(length(a$dependencies), 11)
})
