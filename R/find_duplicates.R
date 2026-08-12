#' Find duplicated entries in metadata
#'
#' `find_duplicates` detect possible duplicated entries from merged
#' metadata from several repositories.
#' @inheritParams template_params
#' @param metadata data frame obtained from combining the output metadata of
#' two or more suwo query function using the `merge_metadata()` function.
#' Data frames obtained from a single suwo query function can also be used
#' but duplicates are not really expected within the same repository. The data
#' frame must have the following columns: `user_name`, `locality`, `repository`,
#'  `country`, `format`, `time`, and `date`.
#' @param sort Logical argument indicating if the output data frame should be
#' sorted by the `duplicate_group` column added by this function. This will
#' group all potential duplicates together in the output data frame.
#' Default is `TRUE`.
#' @param criteria A character string indicating the criteria to use to
#' determine duplicates. By default, the criteria is set to
#' \code{country > 0.8 & locality > 0.5 & user_name > 0.8 & time == 1
#' & date == 1}
#' which means that two entries will be considered duplicates if they have a
#' country similarity greater than 0.8, locality similarity greater than 0.5,
#' user_name similarity greater than 0.8, and exact matches for time and date
#' (similarities range from 0 to 1). These values have been found to work well
#' in most cases. Users can modify this string to adjust the sensitivity of the
#' duplicate detection based on their specific needs.
#' @return A data frame with the input data frame and an additional column
#' named `duplicate_group` indicating potential duplicates with a common index.
#' Entries without potential duplicates are labeled as `NA` in this new column.
#' @export
#' @name find_duplicates
#' @details This function compares the information in the entries of a
#' combined metadata data frame (typically the output of
#' [merge_metadata()]) and labels those possible duplicates with
#' a common index in a new column named `duplicate_group`. The comparison is
#' based on the similarity of the following fields: `user_name`, `locality`,
#' `time` and `country`. Only rows with no missing data for those fields will
#' be considered. The function uses the `RecordLinkage` package to perform the
#' a fuzzy matching comparison and identify potential duplicates based on
#' predefined similarity thresholds (see argument 'criteria'). The function
#' only spots duplicates from different repositories and assumes those
#' duplicates should have the same `format` and `date`. This function is
#' useful for curating the data obtained by merging data from multiple sources,
#' as the same observation might be recorded in different repositories.
#' This is a common issue in citizen science repositories, where users might
#' upload the same observation to different platforms. This can also occur as
#' some repositories automatically share data with other repositories,
#' particularly with GBIF. Note that the function does not remove any entries.
#' The removal of duplicates can be done with the function
#' [remove_duplicates()]. Further identification of duplicates can be done by
#' checking the file size of the media (or including it as a criterion in the
#' 'criteria' argument). To obtain the file size, the media files need to be
#' downloaded first with the function [download_media()] which returns the
#' column `file_size` which can be used for this purpose.
#' @examples \donttest{
#' # get metadata from 2 repos
#' gb <- query_gbif(species = "Turdus rufiventris", format =  "sound")
#' inat <- query_inaturalist(species = "Turdus rufiventris",
#'   format = "sound")
#'
#' # run if queries didnt fail
#'  if (!is.null(gb) && !is.null(inat)) {
#' # combine metadata
#' merged_metadata <- merge_metadata(inat, gb)
#'
#' # find duplicates
#' label_dup_metadata <- find_duplicates(metadata = merged_metadata)
#' }
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
find_duplicates <- function(
  metadata,
  sort = TRUE,
  criteria = paste(
    "country > 0.8",
    "locality > 0.5",
    "user_name > 0.8",
    "time == 1",
    "date == 1",
    sep = " & "
  ),
  verbose = getOption("suwo_verbose", TRUE)
) {
  ##  argument checking
  check_results <- .check_arguments(
    fun = "find_duplicates",
    args = list(
      metadata = metadata,
      sort = sort,
      verbose = verbose
    )
  )

  # report errors
  .report_assertions(check_results)

  if (nrow(metadata) < 2) {
    cli::cli_abort(
      "The input data frame should have at least two rows to compare."
    )
  }

  # index to order back
  metadata$..original_order <- seq_len(nrow(metadata))

  # get criteria columns
  criteria_columns <-
    regmatches(criteria, gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b", criteria))

  criteria_columns <- c(criteria_columns[[1]], "format")

  # keep only those complete cases for used columns
  non_complete_metadata <-
    metadata[
      !stats::complete.cases(metadata[, criteria_columns]),
    ]

  metadata <-
    metadata[
      stats::complete.cases(metadata[, criteria_columns]),
    ]

  # spot duplicates
  similarities <-
    RecordLinkage::compare.dedup(
      metadata[, criteria_columns],
      strcmp = TRUE
    )$pairs

  # remove last column (is_match)
  similarities <- similarities[, -ncol(similarities)]

  # # get repo of each id
  similarities$repo1 <- metadata$repository[similarities$id1]
  similarities$repo2 <- metadata$repository[similarities$id2]

  # add format and complete cases to criteria
  criteria <- paste(
    "with(similarities,",
    criteria,
    "& format == 1 & stats::complete.cases(similarities))"
  )

  # spot duplicates
  possible_duplicates <- similarities[eval(parse(text = criteria)), ]

  # create list with NAS and length nrow(metadata)
  matching_list <- as.list(seq_len(nrow(metadata)))
  repo_list <- as.list(metadata$repository)

  ## create labels to group duplicates
  ## also keep track if their belong to the same repository
  for (i in seq_len(nrow(possible_duplicates))) {
    matching_list[[possible_duplicates$id1[i]]] <- sort(unique(c(
      matching_list[[possible_duplicates$id1[i]]],
      possible_duplicates$id2[i]
    )))
    repo_list[[possible_duplicates$id1[i]]] <- sort(unique(c(
      repo_list[[possible_duplicates$id1[i]]],
      possible_duplicates$repo2[i]
    )))

    matching_list[[possible_duplicates$id2[i]]] <- sort(unique(c(
      matching_list[[possible_duplicates$id2[i]]],
      possible_duplicates$id1[i]
    )))
    repo_list[[possible_duplicates$id2[i]]] <- sort(unique(c(
      repo_list[[possible_duplicates$id2[i]]],
      possible_duplicates$repo1[i]
    )))
  }

  # make those with only 1 value a NA
  matching_list <- vapply(
    matching_list,
    function(x) {
      if (length(x) == 1) {
        as.character(NA)
      } else {
        paste(x, collapse = "-")
      }
    },
    character(1)
  )

  # convert to unique values and add duplicate index to metadata
  metadata$duplicate_group <- as.numeric(as.factor(matching_list))
  non_complete_metadata$duplicate_group <- NA

  # sort by duplicate_group
  if (sort) {
    metadata <- metadata[order(metadata$duplicate_group), ]
  }

  metadata <- rbind(metadata, non_complete_metadata)

  # order back
  if (!sort) {
    metadata <- metadata[order(metadata$..original_order), ]
  }

  # remove original order column
  metadata$..original_order <- NULL

  if (verbose) {
    if (all(is.na(metadata$duplicate_group))) {
      .message(
        "No duplicates were found",
        as = "success",
        suffix = "\n"
      )
    } else {
      .message(
        "{n} potential duplicate{?s} found",
        n = sum(!is.na(metadata$duplicate_group)) -
          length(stats::na.omit(unique(metadata$duplicate_group))),
        as = "message"
      )
    }
  }

  return(metadata)
}
