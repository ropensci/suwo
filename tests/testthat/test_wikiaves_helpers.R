# Unit tests for the pure-logic WikiAves helper functions extracted into
# internal_functions.R (.parse_wikiaves_cookies, .wikiaves_browser_name,
# .strip_trailing_slash), plus the no-cookies-provided early-return path
# in query_wikiaves() itself. None of these require a live browser,
# network access, or valid WikiAves credentials, so unlike
# test_access_wikiaves.R / test_query_wikiaves.R they are expected to run
# fully on CI/CRAN.

test_that(".parse_wikiaves_cookies handles empty/NULL input", {
  expect_identical(suwo:::.parse_wikiaves_cookies(NULL), character(0))
  expect_identical(suwo:::.parse_wikiaves_cookies(""), character(0))
})

test_that(".parse_wikiaves_cookies parses a valid JSON cookies string", {
  cookies_json <- jsonlite::toJSON(
    list(
      cf_clearance = "abc123",
      PHPSESSID = "def456",
      browser_ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"
    ),
    auto_unbox = TRUE
  )

  parsed <- suwo:::.parse_wikiaves_cookies(cookies_json)

  expect_true(is.character(parsed))
  expect_equal(unname(parsed["cf_clearance"]), "abc123")
  expect_equal(unname(parsed["PHPSESSID"]), "def456")
  expect_equal(
    unname(parsed["browser_ua"]),
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"
  )
})

test_that(".parse_wikiaves_cookies errors clearly on malformed JSON", {
  expect_error(
    suwo:::.parse_wikiaves_cookies("not valid json{{{"),
    class = "wikiaves_cookies_parse_error"
  )
})

test_that(".wikiaves_browser_name detects Chrome", {
  expect_equal(suwo:::.wikiaves_browser_name("google-chrome"), "Chrome")
  expect_equal(suwo:::.wikiaves_browser_name("google-chrome-stable"), "Chrome")
  expect_equal(
    suwo:::.wikiaves_browser_name("/usr/bin/google-chrome"),
    "Chrome"
  )
  expect_equal(
    suwo:::.wikiaves_browser_name(
      "C:/Program Files/Google/Chrome/Application/chrome.exe"
    ),
    "Chrome"
  )
})

test_that(".wikiaves_browser_name detects Chromium", {
  expect_equal(suwo:::.wikiaves_browser_name("chromium"), "Chromium")
  expect_equal(suwo:::.wikiaves_browser_name("chromium-browser"), "Chromium")
  expect_equal(
    suwo:::.wikiaves_browser_name(
      "/Applications/Chromium.app/Contents/MacOS/Chromium"
    ),
    "Chromium"
  )
})

test_that(".wikiaves_browser_name falls back to the raw name for unknown binaries", {
  expect_equal(
    suwo:::.wikiaves_browser_name("my-custom-browser"),
    "my-custom-browser"
  )
  expect_equal(
    suwo:::.wikiaves_browser_name("/opt/browsers/my-custom-browser"),
    "my-custom-browser"
  )
})

test_that(".strip_trailing_slash removes trailing slashes", {
  expect_equal(
    suwo:::.strip_trailing_slash("https://example.com/"),
    "https://example.com"
  )
  expect_equal(
    suwo:::.strip_trailing_slash("https://example.com"),
    "https://example.com"
  )
  expect_equal(
    suwo:::.strip_trailing_slash("https://example.com///"),
    "https://example.com"
  )
  # only trailing slashes are stripped, not slashes elsewhere in the URL
  expect_equal(
    suwo:::.strip_trailing_slash("https://example.com/path/"),
    "https://example.com/path"
  )
})

test_that("query_wikiaves() returns NULL gracefully when no cookies are provided", {
  # this exercises query_wikiaves()'s own no-cookies early-return path
  # without ever reaching a network call, so it can run on CI/CRAN
  original_env <- Sys.getenv("wikiaves_cookies", unset = NA)
  on.exit(
    if (is.na(original_env)) {
      Sys.unsetenv("wikiaves_cookies")
    } else {
      Sys.setenv(wikiaves_cookies = original_env)
    },
    add = TRUE
  )
  Sys.unsetenv("wikiaves_cookies")

  expect_null(
    suppressMessages(
      query_wikiaves(
        species = "Glaucis dohrnii",
        format = "sound",
        cookies = ""
      )
    )
  )
})

test_that("query_wikiaves() fails gracefully (not a crash) with fake cookies", {
  # Unlike the other WikiAves tests, this one only needs real internet
  # access -- NOT a real, valid wikiaves_cookies session. Cloudflare
  # rejects fake/expired credentials with an HTTP 403 regardless, so this
  # exercises the request-building + graceful-failure-on-403 path (the
  # exact bug fixed earlier, where an uncaught httr2 error used to crash
  # the function instead of returning NULL with a clear message).
  skip_on_cran()
  skip_if_offline()

  fake_cookies <- jsonlite::toJSON(
    list(
      cf_clearance = "not_a_real_value",
      PHPSESSID = "not_a_real_session",
      browser_ua = paste(
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36",
        "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
      )
    ),
    auto_unbox = TRUE
  )

  result <- NULL
  msg <- testthat::capture_messages(
    result <- query_wikiaves(
      species = "Glaucis dohrnii",
      format = "sound",
      cookies = fake_cookies
    )
  )

  expect_null(result)
  expect_true(any(grepl("request failed|expired", msg, ignore.case = TRUE)))
})

test_that("access_wikiaves() validates arguments before touching network or browser", {
  # These all fail argument validation (.check_arguments() /
  # .report_assertions()) immediately -- before the connection check,
  # package-install prompt, or any browser/network code runs -- so they
  # can run anywhere, with no network, browser, or credentials required.

  expect_error(access_wikiaves(port = "not_a_port"))
  expect_error(access_wikiaves(port = -1))
  expect_error(access_wikiaves(port = 99999))
  expect_error(access_wikiaves(url = "not-a-url"))
  expect_error(access_wikiaves(timeout = -5))
  expect_error(access_wikiaves(launch_wait = -1))
  expect_error(access_wikiaves(challenge_wait = "thirty"))
  expect_error(access_wikiaves(chrome_bin = ""))
  expect_error(access_wikiaves(user_data_dir = ""))
  expect_error(access_wikiaves(set_env = "yes"))
})
