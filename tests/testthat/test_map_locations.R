options(verbose = TRUE)


test_that("map Helicobacter pylori", {
  df1 <- suwo:::testing_metadata$t_rufiventris

  a <- map_locations(metadata = df1, cluster = TRUE)

  expect_true(class(a)[1] == "leaflet")

  expect_true(class(a)[2] == "htmlwidget")
})
