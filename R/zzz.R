# load default options on package load
.onLoad <- function(libname, pkgname) {
  # Store original options to restore later
  op <- options()

  # Package-specific default options
  op.yourpackage <- list(
    suwo_cores = 1,
    suwo_pb = TRUE,
    suwo_verbose = TRUE,
    suwo_all_data = FALSE,
    suwo_raw_data = FALSE
  )

  # Only set options that haven't been set by user
  toset <- !(names(op.yourpackage) %in% names(op))
  if (any(toset)) {
    options(op.yourpackage[toset])
  }

  # Store original state for restoration
  assign(".original_options", op, envir = parent.env(environment()))

  invisible()
}

.onUnload <- function(libpath) {
  # Get original options
  original_op <- get(".original_options", envir = parent.env(environment()))

  # Restore only the options that were modified by our package
  package_options <-
    c("suwo_cores", "suwo_pb", "suwo_verbose", "suwo_all_data", "suwo_raw_data")

  for (opt in package_options) {
    if (opt %in% names(original_op)) {
      # Restore original value
      do.call(options, stats::setNames(list(original_op[[opt]]), opt))
    } else {
      # Option didn't exist before, so remove it
      do.call(options, stats::setNames(list(NULL), opt))
    }
  }

  # Clean up
  if (exists(".original_options", envir = parent.env(environment()))) {
    try(
      rm(".original_options", envir = parent.env(environment())),
      silent = TRUE
    )
  }

  invisible()
}


# message when loading package
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    cit <- format(utils::citation(pkgname)[1], style = "text")

    packageStartupMessage(
      "\nPlease cite '",
      pkgname,
      "' as:\n",
      paste(cit, collapse = "\n"),
      "\n"
    )
  }
}
