#' Merge metadata data frames
#'
#' `merge_metadata` merges metadata data frames from suwo queries.
#' @param ... two or more data frames (each one as a separate entry) referring
#' to the metadata obtained from suwo query functions (`query_x()`).
#' Alternatively, a single list of data frames can be provided. The name
#' provided for each data frame (either as individual data frames or in a list)
#' will be used as label in the `source` column in the output data frame.
#' @param check_columns Logical argument indicating if the function should
#' check that all input data frames have the required basic columns.
#' Default is `TRUE`.
#' @return A single data frame with the data from all input data frames
#' combined and with an additional column named `source` indicating the
#' original data frame from which each row originated. The column `source` will
#'  contain the name provided for each data frame (either as individual data
#'  frames or in a list). If no names were provided, the object names will
#'  be used instead.
#' @export
#' @name merge_metadata
#' @details This function combines metadata from multiple sources
#' (e.g. WikiAves and xeno-canto) into a single data frame for easier analysis
#' and comparison. Each input data frame must be obtained from one of the
#' suwo query functions (e.g., `query_wikiaves()`, `query_xenocanto()`, etc.)
#' with `raw_data = FALSE`.
#' @examples \donttest{
#' # get metadata from 2 repos
#' wa <- query_wikiaves(species = "Glaucis dohrnii", format =  "sound")
#' gb <- query_gbif(species = "Glaucis dohrnii", format = "sound")
#'
#' # run if queries didnt fail
#'  if (!is.null(wa) && !is.null(gb)) {
#'  # combine metadata using single data frames
#'  merged_mt <- merge_metadata(wa, gb)
#'
#'  # combine metadata using named single data frames
#'  merged_mt <- merge_metadata(wikiaves = wa, gbif = gb)
#'
#'  # combine metadata using a list of data frames
#'  mt_list <- list(wikiaves = wa, gbif = gb)
#'  merged_mt <- merge_metadata(mt_list)
#' }
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
merge_metadata <- function(..., check_columns = TRUE) {
  dots <- list(...)
  dot_names <- names(dots)

  ## Drop NULL inputs
  is_null <- vapply(dots, is.null, logical(1))

  if (all(is_null)) {
    stop("All inputs are NULL. Nothing to merge.", call. = FALSE)
  }

  dots <- dots[!is_null]
  dot_names <- dot_names[!is_null]

  # Detect if user passed a single list of data frames
  if (
    length(dots) == 1 &&
      is.list(dots[[1]]) &&
      !is.data.frame(dots[[1]])
  ) {
    metadata_list <- dots[[1]]
    from_list <- TRUE
  } else {
    metadata_list <- dots
    from_list <- FALSE
  }

  # Validate all inputs are data frames
  for (i in seq_along(metadata_list)) {
    if (!is.data.frame(metadata_list[[i]])) {
      stop(
        "All inputs must be data frames or a list of data frames.",
        call. = FALSE
      )
    }
  }

  ## Determine source names
  if (from_list) {
    if (
      !is.null(names(metadata_list)) &&
        any(nzchar(names(metadata_list)))
    ) {
      df_names <- names(metadata_list)
    } else {
      df_names <- paste0("df", seq_along(metadata_list))
    }
  } else {
    if (!is.null(dot_names) && any(nzchar(dot_names))) {
      df_names <- dot_names
    } else {
      call <- match.call()
      args <- as.list(call)[-1]
      df_names <- vapply(
        args,
        function(x) {
          as.character(deparse(x))
        },
        character(1)
      )
    }
  }

  ## Check for duplicate names
  if (anyDuplicated(df_names)) {
    warning(
      "Duplicate names detected in input: ",
      paste(unique(df_names[duplicated(df_names)]), collapse = ", "),
      call. = FALSE
    )
  }

  ## check required columns
  if (check_columns) {
    basic_out <- .format_query_output(only_basic_columns = TRUE)

    # Robust extraction of required column names
    if (is.data.frame(basic_out)) {
      required_cols <- colnames(basic_out)
    } else if (is.list(basic_out) && !is.null(names(basic_out))) {
      required_cols <- names(basic_out)
    } else if (is.character(basic_out)) {
      required_cols <- basic_out
    } else {
      stop(
        paste(
          "Could not determine required basic columns",
          "from .format_query_output()."
        ),
        call. = FALSE
      )
    }

    if (length(required_cols) == 0) {
      stop(
        "No required basic columns were returned by .format_query_output().",
        call. = FALSE
      )
    }

    # Validate each df
    for (i in seq_along(metadata_list)) {
      missing_cols <- setdiff(required_cols, names(metadata_list[[i]]))
      if (length(missing_cols) > 0) {
        stop(
          sprintf(
            "Data frame '%s' is missing required columns: %s",
            df_names[i],
            paste(missing_cols, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  } # end required-column check

  ## Add source column
  for (i in seq_along(metadata_list)) {
    metadata_list[[i]]$source <- df_names[i]
  }

  ## merge data frames
  merged_data <- .merge_data_frames(metadata_list)

  return(merged_data)
}
