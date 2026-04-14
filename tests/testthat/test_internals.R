## date ranges
test_that(".date_ranges", {
  dts <- suwo:::.date_ranges(x = c(1976, 2020, 2022, 2024, 2025, 2026))

  # test
  expect_true(nrow(dts) == 5)

  dts <- suwo:::.date_ranges(x = seq(1976, 2026, length.out = 50))

  expect_true(nrow(dts) == 95)
})

test_that(".onLoad", {
  dts <- try(suwo:::.onLoad(), silent = TRUE)

  expect_true(base::inherits(dts, "try-error"))
})

test_that(".onUnload", {
  dts <- try(suwo:::.onUnload(), silent = TRUE)

  expect_true(is.null(dts))
})


test_that(".add_emoji", {
  em <- suwo:::.add_emoji("happy")

  expect_true(class(em) == "character")
})


test_that(".repo_from_call handles namespaced calls", {
  skip_on_cran()
  skip_if_offline()

  # construct call objects
  call_non_ns <- quote(query_gbif(
    species = 'Aristolochia baetica',
    format = "image"
  ))
  call_ns <- quote(suwo::query_gbif(
    species = 'Aristolochia baetica',
    format = "image"
  ))

  # call the internal function using the namespace
  f <- suwo:::.repo_from_call

  expect_identical(
    f(call_non_ns),
    "GBIF"
  )

  expect_identical(
    f(call_ns),
    "GBIF"
  )
})

test_that("pb apply with parallel", {
  fun2 <- function(x, Y) {
    Sys.sleep(0.1)
    return(Y[x] * 0.5)
  }

  X <- 1:35

  a2 <- .pbapply_sw(
    X = X,
    FUN = function(x, Y = 1:35) {
      Sys.sleep(0.1)
      return(Y[x] * 0.5)
    },
    title = "ahi",
    cl = 1,
    pbar = F
  )
  a3 <- .pbapply_sw(
    X = X,
    FUN = function(x, Y = 1:35) {
      Sys.sleep(0.1)
      return(Y[x] * 0.5)
    },
    title = "ahi",
    cl = 5,
    pbar = F
  )
  a5 <- .pbapply_sw(
    X = X,
    FUN = function(x, Y = 1:35) {
      Sys.sleep(0.1)
      return(Y[x] * 0.5)
    },
    cl = 1,
    pbar = TRUE
  )
  a4 <- .pbapply_sw(
    X = 1:55,
    FUN = function(x, Y = 11:65) {
      Sys.sleep(0.1)
      return(Y[x] * 0.5)
    },
    cl = 5,
    pbar = TRUE
  )

  expect_equal(unlist(a2), unlist(a3))
  expect_equal(unlist(a5), unlist(a3))
})
