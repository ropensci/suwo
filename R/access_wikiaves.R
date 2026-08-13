#' Obtain WikiAves authentication cookies via a running Chrome or Chromium instance
#'
#' @description
#' `access_wikiaves` authenticates with [WikiAves](https://www.wikiaves.com.br),
#' which sits behind Cloudflare's bot protection and blocks plain `httr2`
#' requests with an HTTP 403 and a "Just a moment..." challenge page. This
#' function works around that by driving a real, visible Chromium-based
#' browser (Google Chrome, Chromium, or another compatible build) through
#' the Chrome DevTools Protocol (CDP): it launches the browser (or reuses
#' one already running) pointed at WikiAves, waits for Cloudflare's
#' JavaScript challenge to clear, and then extracts the resulting cookies
#' and the user agent string. The values are packed into a single
#' character string.
#'
#' By default (`set_env = TRUE`) this string is also stored directly in
#' the `wikiaves_cookies` environment variable for the current R session,
#' which is what [query_wikiaves()] reads from by default -- so in the
#' common case, calling `access_wikiaves()` once is enough to authenticate
#' every subsequent [query_wikiaves()] call in the session without passing
#' `cookies` explicitly. See Details.
#'
#' This function works on Linux, macOS, and Windows. On all three
#' platforms it will try to auto-detect a suitable browser and a writable
#' temporary directory if `chrome_bin` and `user_data_dir` are left at
#' their default (`NULL`); see Details.
#'
#' @param chrome_bin Character. Name or path of the browser executable to
#'   launch. Default `NULL`, which triggers auto-detection of a Chrome- or
#'   Chromium-based browser for the current operating system (see
#'   Details). Set this explicitly to override auto-detection or to point
#'   at a non-standard install location, e.g. `access_wikiaves("chromium")`
#'   on Linux, `"/Applications/Chromium.app/Contents/MacOS/Chromium"` on
#'   macOS, or `"C:/Program Files/Google/Chrome/Application/chrome.exe"`
#'   on Windows. Messages printed while the function runs refer to
#'   whichever browser `chrome_bin` resolves to, so it is easy to tell
#'   which one is in use.
#' @param port Integer. The TCP port used for the browser's DevTools
#'   remote debugging protocol. Default `9333`. If a browser instance is
#'   already listening on this port, it is reused instead of launching a
#'   new one.
#' @param url Character. The WikiAves URL to navigate to and extract
#'   cookies from. Default `"https://www.wikiaves.com.br/"`. Changing this
#'   is only useful for testing against a different page on the same
#'   domain.
#' @param timeout Numeric. Maximum time, in seconds, to wait for a single
#'   round-trip response (cookies, user agent, and page title) over the
#'   DevTools websocket connection. Default `10`.
#' @param launch_wait Numeric. Maximum time, in seconds, to wait for a
#'   newly launched browser process to open its DevTools debugging port.
#'   Default `15`. Only relevant when the browser is not already running
#'   on `port`.
#' @param challenge_wait Numeric. Maximum total time, in seconds, to keep
#'   polling the page while Cloudflare's "Just a moment..." challenge is
#'   showing. Default `30`. If the challenge has not cleared within this
#'   window the function stops with an informative error; the person can
#'   solve the challenge manually in the visible browser window (e.g.
#'   clicking a checkbox) while this function is polling, and it will
#'   pick up the cleared page on the next check.
#' @param user_data_dir Character. Filesystem path to the browser's user
#'   data directory used for this session. Default `NULL`, which resolves
#'   to a `"chrome-wikiaves-profile"` folder inside `tempdir()` -- a
#'   location valid on Linux, macOS, and Windows alike. Reusing the same
#'   directory across calls lets the browser and Cloudflare recognize the
#'   profile as previously trusted, which often lets later calls clear
#'   the challenge automatically without manual interaction.
#' @param set_env Logical. If `TRUE` (the default), also stores the
#'   returned cookies string in the `wikiaves_cookies` environment
#'   variable for the current R session via `Sys.setenv()`, so that
#'   [query_wikiaves()]'s default `cookies` argument picks it up
#'   automatically without needing to pass it explicitly. Set to `FALSE`
#'   to make the function a pure getter that only returns the cookies
#'   string without modifying the session's environment variables --
#'   useful if you want to manage the credentials yourself (e.g. pass them
#'   directly into a single [query_wikiaves()] call, store them somewhere
#'   else, or use a different session's variable).
#'
#' @details
#' This function requires the **websocket** and **later** packages, which
#' are listed under Suggests rather than Imports since this workflow is
#' only needed for the WikiAves data source and depends on a local Chrome-
#' or Chromium-based browser installation and a usable display (e.g. the
#' `DISPLAY` environment variable on Linux). If either package is missing,
#' in an interactive session the function asks for permission before
#' installing them via `install.packages()`; declining stops the function
#' with instructions to install manually. In a non-interactive session
#' (e.g. within `R CMD check`, CI, or `Rscript`), the function cannot
#' prompt and instead stops immediately with the same instructions.
#'
#' **Browser auto-detection.** When `chrome_bin = NULL` (the default), the
#' function first checks whether the **chromote** package happens to be
#' installed and, if so, uses its exported `chromote::find_chrome()` to
#' locate a browser -- this respects the `CHROMOTE_CHROME` environment
#' variable and **chromote**'s own per-OS search logic. **chromote** is
#' not a dependency of this function; if it is not installed, or its
#' detection does not find anything, the function falls back to its own
#' candidate-path search:
#' - **Linux**: `google-chrome`, `google-chrome-stable`, `chromium`, and
#'   `chromium-browser`, in that order, checked via `Sys.which()`.
#' - **macOS**: `/Applications/Google Chrome.app/Contents/MacOS/Google Chrome`
#'   and `/Applications/Chromium.app/Contents/MacOS/Chromium`.
#' - **Windows**: the `chrome.exe` install locations under `Program Files`
#'   and `Program Files (x86)`, as well as a per-user AppData install
#'   location.
#'
#' If none of these are found, the function stops with an error explaining
#' how to set `chrome_bin` manually. Auto-detected paths have only been
#' tested against typical default install locations; if your browser is
#' installed somewhere non-standard, pass its path explicitly via
#' `chrome_bin`.
#'
#' On first use (or once the session has expired, which typically happens
#' within a couple of hours), Cloudflare may present a visible interactive
#' challenge in the launched browser window. Switching to that window and
#' completing the challenge is a normal part of the workflow; this
#' function polls for up to `challenge_wait` seconds so the person has
#' time to do so before it gives up, and prints a message once the
#' cookies have been successfully retrieved.
#'
#' **Automatic environment variable.** By default (`set_env = TRUE`), the
#' returned cookies string is also written to
#' `Sys.setenv(wikiaves_cookies = ...)` before the function returns. Since
#' [query_wikiaves()]'s `cookies` argument defaults to
#' `Sys.getenv("wikiaves_cookies")`, this means a bare `access_wikiaves()`
#' call is normally all that is needed to authenticate every
#' [query_wikiaves()] call made afterwards in the same R session -- there
#' is no need to manually capture and pass the return value unless
#' `set_env = FALSE` is used.
#'
#' @return
#' A single JSON-encoded character string containing `cf_clearance`,
#' `PHPSESSID`, and `browser_ua`. This is the format expected by the
#' `cookies` argument of [query_wikiaves()]. The string is returned
#' invisibly if `set_env = TRUE` (its side effect -- setting
#' `wikiaves_cookies` -- is normally what matters in that case), and
#' visibly if `set_env = FALSE`. Not expected to be modified by the user.
#' Returns `invisible(NULL)` instead if no internet connection is
#' available.
#'
#' @seealso [query_wikiaves()], which accepts the output of this function
#'   via its `cookies` argument.
#'
#' @examples
#' if (interactive()) {
#' # Simplest usage: launches (or reuses) a browser instance,
#' # auto-detecting an appropriate Chrome/Chromium binary for the current
#' # OS, solves the Cloudflare challenge if needed, and stores the
#' # resulting cookies in the `wikiaves_cookies` environment variable
#' # (set_env = TRUE by default) -- so query_wikiaves() picks them up
#' # automatically without passing `cookies` explicitly:
#' access_wikiaves()
#' result <- query_wikiaves(species = "Procnias averano", format = "sound")
#'
#' # chrome_bin is the first argument, so a non-default browser binary can
#' # be supplied positionally:
#' access_wikiaves("chromium")
#'
#' # Using a non-default debugging port as well:
#' access_wikiaves("chromium", port = 9444)
#'
#' # set_env = FALSE turns access_wikiaves() into a pure getter: nothing
#' # is stored automatically, and the cookies string must be captured and
#' # passed to query_wikiaves() (or elsewhere) manually:
#' cookies_live <- access_wikiaves(set_env = FALSE)
#' result <- query_wikiaves(
#'   species = "Procnias averano",
#'   format = "sound",
#'   cookies = cookies_live
#' )
#' }
#'
#' @export
access_wikiaves <- function(
  chrome_bin = NULL,
  port = 9333,
  url = "https://www.wikiaves.com.br/",
  timeout = 10,
  launch_wait = 15,
  challenge_wait = 30,
  user_data_dir = NULL,
  set_env = TRUE
) {
  ## argument checking
  check_results <- .check_arguments(
    fun = "access_wikiaves",
    args = list(
      chrome_bin = chrome_bin,
      port = port,
      url = url,
      timeout = timeout,
      launch_wait = launch_wait,
      challenge_wait = challenge_wait,
      user_data_dir = user_data_dir,
      set_env = set_env
    )
  )

  # report errors
  .report_assertions(check_results)

  # Use the unified connection checker to fail fast (before launching a
  # browser) if there is no internet connection at all. Note
  # .checkconnection() treats an HTTP 403 from WikiAves as "connected"
  # rather than "down", since Cloudflare blocks plain requests with a 403
  # even when the site is perfectly healthy -- see .checkconnection() for
  # details.
  if (!.checkconnection(verb = TRUE, service = "wikiaves")) {
    return(invisible(NULL))
  }

  ## check for required optional packages, offering to install them
  ## interactively rather than just stopping
  required_pkgs <- c("websocket", "later")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    install_cmd <- paste0(
      "install.packages(c(",
      paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
      "))"
    )

    if (interactive()) {
      ans <- utils::menu(
        choices = c("Yes", "No"),
        title = paste0(
          "The following package(s) are required but not installed: ",
          paste(missing_pkgs, collapse = ", "),
          ".\nInstall them now?"
        )
      )

      if (ans == 1) {
        utils::install.packages(missing_pkgs)

        # re-check in case installation failed silently for any of them
        still_missing <- missing_pkgs[
          !vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)
        ]

        if (length(still_missing) > 0) {
          stop(
            "Failed to install: ",
            paste(still_missing, collapse = ", "),
            ". Please install manually with:\n  ",
            install_cmd,
            call. = FALSE
          )
        }
      } else {
        stop(
          "Cannot proceed without required package(s). Install them with:\n  ",
          install_cmd,
          call. = FALSE
        )
      }
    } else {
      # non-interactive sessions (CI, R CMD check, Rscript, etc.) cannot
      # be prompted, so fail immediately with clear instructions instead
      stop(
        "Package(s) '",
        paste(missing_pkgs, collapse = "', '"),
        "' are required for this function. Install them with:\n  ",
        install_cmd,
        call. = FALSE
      )
    }
  }

  os_type <- .Platform$OS.type # "windows" or "unix"
  sys_name <- Sys.info()[["sysname"]] # "Windows", "Darwin", or "Linux"

  # ---- auto-detect a browser binary if the user did not supply one -------
  # Prefer chromote's own detection (chromote::find_chrome()) when the
  # chromote package happens to be installed: it already handles the
  # CHROMOTE_CHROME environment variable override and per-OS search paths,
  # and is maintained/tested by the chromote authors across real machines.
  # chromote is NOT a hard dependency of this function -- if it is not
  # installed, or find_chrome() returns nothing, we fall back to the
  # vendored candidate-path search below.
  .find_chrome_via_chromote <- function() {
    if (!requireNamespace("chromote", quietly = TRUE)) {
      return(NULL)
    }
    result <- tryCatch(unname(chromote::find_chrome()), error = function(e) {
      NULL
    })
    if (is.null(result) || !nzchar(result)) NULL else result
  }

  .default_chrome_bin <- function() {
    via_chromote <- .find_chrome_via_chromote()
    if (!is.null(via_chromote)) {
      return(via_chromote)
    }

    candidates <- if (sys_name == "Windows") {
      c(
        file.path(
          Sys.getenv("PROGRAMFILES"),
          "Google/Chrome/Application/chrome.exe"
        ),
        file.path(
          Sys.getenv("PROGRAMFILES(X86)"),
          "Google/Chrome/Application/chrome.exe"
        ),
        file.path(
          Sys.getenv("LOCALAPPDATA"),
          "Google/Chrome/Application/chrome.exe"
        ),
        file.path(Sys.getenv("PROGRAMFILES"), "Chromium/Application/chrome.exe")
      )
    } else if (sys_name == "Darwin") {
      c(
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
        "/Applications/Chromium.app/Contents/MacOS/Chromium"
      )
    } else {
      # Linux and other Unix-likes: rely on PATH lookups
      c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser")
    }

    for (candidate in candidates) {
      if (
        nzchar(candidate) &&
          (file.exists(candidate) || nzchar(Sys.which(candidate)))
      ) {
        return(candidate)
      }
    }

    stop(
      "Could not auto-detect a Chrome/Chromium installation. ",
      "Set the `chrome_bin` argument to the path of your browser executable, e.g.:\n",
      if (sys_name == "Windows") {
        '  access_wikiaves(chrome_bin = "C:/Program Files/Google/Chrome/Application/chrome.exe")'
      } else if (sys_name == "Darwin") {
        '  access_wikiaves(chrome_bin = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")'
      } else {
        '  access_wikiaves(chrome_bin = "chromium")'
      },
      call. = FALSE
    )
  }

  if (is.null(chrome_bin)) {
    chrome_bin <- .default_chrome_bin()
  }

  if (is.null(user_data_dir)) {
    user_data_dir <- file.path(tempdir(), "chrome-wikiaves-profile")
  }

  # human-readable label for messages, derived from chrome_bin so users see
  # the name of the browser they are actually trying to use (Chrome,
  # Chromium, or whatever else was supplied) rather than a hardcoded name
  browser_name <- basename(chrome_bin)
  browser_name <- sub("\\.exe$", "", browser_name, ignore.case = TRUE)
  browser_name <- if (grepl("chromium", browser_name, ignore.case = TRUE)) {
    "Chromium"
  } else if (grepl("chrome", browser_name, ignore.case = TRUE)) {
    "Chrome"
  } else {
    browser_name
  }

  # platform-appropriate "discard output" redirection target
  null_device <- if (os_type == "windows") "NUL" else "/dev/null"

  # check if a DevTools instance is already reachable on `port`
  .chrome_reachable <- function() {
    tryCatch(
      {
        suppressWarnings(
          jsonlite::fromJSON(paste0("http://localhost:", port, "/json"))
        )
        TRUE
      },
      error = function(e) FALSE
    )
  }

  # fetch cookies, user agent, and page title from a given DevTools
  # websocket URL via the Chrome DevTools Protocol (CDP)
  .fetch_via_cdp <- function(ws_url) {
    ws <- websocket::WebSocket$new(ws_url, autoConnect = FALSE)

    cookie_result <- NULL
    ua_result <- NULL
    title_result <- NULL
    ws_error <- NULL

    ws$onMessage(function(event) {
      msg <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
      if (!is.null(msg$id) && msg$id == 1) {
        cookie_result <<- msg$result$cookies
      }
      if (!is.null(msg$id) && msg$id == 2) {
        ua_result <<- msg$result$result$value
      }
      if (!is.null(msg$id) && msg$id == 3) {
        title_result <<- msg$result$result$value
      }
    })

    ws$onOpen(function(event) {
      ws$send(jsonlite::toJSON(
        list(
          id = 1,
          method = "Network.getCookies",
          params = list(urls = list(url))
        ),
        auto_unbox = TRUE
      ))
      ws$send(jsonlite::toJSON(
        list(
          id = 2,
          method = "Runtime.evaluate",
          params = list(expression = "navigator.userAgent")
        ),
        auto_unbox = TRUE
      ))
      ws$send(jsonlite::toJSON(
        list(
          id = 3,
          method = "Runtime.evaluate",
          params = list(expression = "document.title")
        ),
        auto_unbox = TRUE
      ))
    })

    ws$onError(function(event) ws_error <<- event$message)
    ws$connect()

    start <- Sys.time()
    while (as.numeric(Sys.time() - start, units = "secs") < timeout) {
      later::run_now(timeoutSecs = 0.1)
      Sys.sleep(0.1)
      if (
        !is.null(cookie_result) && !is.null(ua_result) && !is.null(title_result)
      ) {
        break
      }
      if (!is.null(ws_error)) break
    }
    ws$close()

    if (!is.null(ws_error)) {
      stop("WebSocket error while fetching cookies: ", ws_error)
    }
    if (is.null(cookie_result) || is.null(ua_result)) {
      stop(
        "Timed out waiting for cookies/UA from ",
        browser_name,
        " DevTools."
      )
    }

    list(cookies = cookie_result, ua = ua_result, title = title_result)
  }

  # 1. launch the browser if needed, with all its noisy output discarded.
  # Note: `url` is deliberately NOT passed as a launch argument here -- see
  # step 2 below for why.
  if (!.chrome_reachable()) {
    .message(
      paste0(
        browser_name,
        " not found on port ",
        port,
        " -- launching it now..."
      ),
      as = "message"
    )

    # quote chrome_bin too: on macOS/Windows the executable path commonly
    # contains spaces (e.g. "Google Chrome.app", "Program Files")
    cmd <- sprintf(
      '"%s" --remote-debugging-port=%d --user-data-dir="%s" > %s 2>&1',
      chrome_bin,
      port,
      user_data_dir,
      null_device
    )
    system(cmd, wait = FALSE)

    start <- Sys.time()
    while (!.chrome_reachable()) {
      if (as.numeric(Sys.time() - start, units = "secs") > launch_wait) {
        stop(
          browser_name,
          " did not become reachable on port ",
          port,
          " within ",
          launch_wait,
          " seconds. ",
          "Check that '",
          chrome_bin,
          "' is a valid, working browser executable, ",
          "or launch it manually with:\n  ",
          cmd
        )
      }
      Sys.sleep(0.5)
    }
  }

  # 2. find an existing tab already at `url`, or explicitly open one via
  # the DevTools HTTP API. Passing `url` as a launch argument (the
  # previous approach) only works when this call actually launched a new
  # browser process -- if a browser was already running on `port` (e.g.
  # left over from an earlier call in the same session), step 1 above is
  # skipped entirely and nothing ever tells that existing browser to
  # navigate anywhere, so the target tab would never appear no matter how
  # long we waited. Explicitly requesting a tab via the API works
  # identically in both cases.
  .strip_trailing_slash <- function(x) sub("/+$", "", x)
  url_normalized <- .strip_trailing_slash(url)

  .find_existing_tab <- function() {
    targets <- suppressWarnings(jsonlite::fromJSON(paste0(
      "http://localhost:",
      port,
      "/json"
    )))
    if (is.null(targets) || !is.data.frame(targets) || nrow(targets) == 0) {
      return(NULL)
    }
    matches <- targets[.strip_trailing_slash(targets$url) == url_normalized, ]
    if (nrow(matches) > 0) matches[1, ] else NULL
  }

  .create_tab <- function() {
    req <- httr2::request(paste0("http://localhost:", port, "/json/new?", url))
    req <- httr2::req_method(req, "PUT")
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
  }

  # briefly poll for an existing tab first (covers the fast, common case
  # of a freshly launched browser that is still finishing its first
  # navigation); fall back to explicitly creating one if none turns up
  start <- Sys.time()
  wa_target <- NULL
  repeat {
    wa_target <- .find_existing_tab()
    if (!is.null(wa_target)) {
      break
    }
    if (as.numeric(Sys.time() - start, units = "secs") > min(launch_wait, 5)) {
      break
    }
    Sys.sleep(0.3)
  }

  if (is.null(wa_target)) {
    new_tab <- tryCatch(.create_tab(), error = function(e) NULL)
    if (is.null(new_tab) || is.null(new_tab$webSocketDebuggerUrl)) {
      stop(
        "Could not open or find a tab at ",
        url,
        " in the running ",
        browser_name,
        " instance on port ",
        port,
        "."
      )
    }
    ws_url <- new_tab$webSocketDebuggerUrl
  } else {
    ws_url <- wa_target$webSocketDebuggerUrl[1]
  }

  # 3. poll until the Cloudflare challenge clears AND the cookies we
  # actually need are present -- checking only document.title is not
  # enough, since right after the tab is created the title can briefly be
  # blank/generic (page still loading) even though cf_clearance and
  # PHPSESSID have not been set by Cloudflare yet
  start <- Sys.time()
  result <- NULL
  warned <- FALSE
  required_cookies <- c("cf_clearance", "PHPSESSID")

  repeat {
    result <- .fetch_via_cdp(ws_url)

    is_challenge <- !is.null(result$title) &&
      grepl("Just a moment", result$title, ignore.case = TRUE)

    cookie_names <- vapply(result$cookies, function(x) x$name, character(1))
    has_required_cookies <- all(required_cookies %in% cookie_names)

    if (!is_challenge && has_required_cookies) {
      break
    }

    elapsed <- as.numeric(Sys.time() - start, units = "secs")
    if (elapsed > challenge_wait) {
      if (is_challenge) {
        stop(
          "Still on the Cloudflare challenge page after ",
          challenge_wait,
          "s. Switch to the ",
          browser_name,
          " window, solve it manually, then re-run this function."
        )
      } else {
        stop(
          "Page loaded but required cookies (",
          paste(setdiff(required_cookies, cookie_names), collapse = ", "),
          ") were not found after ",
          challenge_wait,
          "s. ",
          "Try reloading ",
          url,
          " in the ",
          browser_name,
          " window, then re-run this function."
        )
      }
    }

    if (!warned && is_challenge) {
      .message(
        paste0(
          "On Cloudflare challenge page -- waiting up to ",
          challenge_wait,
          "s for it to clear (solve manually in the ",
          browser_name,
          " window)"
        ),
        as = "message"
      )
      warned <- TRUE
    }
    Sys.sleep(2)
  }

  cookies_out <- stats::setNames(
    vapply(result$cookies, function(x) x$value, character(1)),
    vapply(result$cookies, function(x) x$name, character(1))
  )

  cookies_out <- c(
    cookies_out[required_cookies],
    browser_ua = result$ua
  )

  .message(
    paste0("Cookies successfully retrieved from ", browser_name, "."),
    as = "success"
  )

  # pack into a single JSON string. By default this is stored directly via
  # Sys.setenv(wikiaves_cookies = ...) below (set_env = TRUE); it is also
  # returned so that set_env = FALSE callers, or anyone who wants to
  # manage the string themselves (e.g. Sys.setenv(wikiaves_cookies = cookies_live)),
  # can do so. JSON (rather than a hand-rolled "key=value;..." format) is
  # used here because browser_ua commonly contains literal semicolons
  # (e.g. "Mozilla/5.0 (X11; Linux x86_64) ..."), which would otherwise be
  # ambiguous with a semicolon-based pair separator.
  cookies_json <- jsonlite::toJSON(as.list(cookies_out), auto_unbox = TRUE)

  if (set_env) {
    Sys.setenv(wikiaves_cookies = cookies_json)
    .message(
      "Saved to the `wikiaves_cookies` environment variable for this session.",
      as = "success"
    )
    return(invisible(cookies_json))
  }

  cookies_json
}
