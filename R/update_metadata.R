#' Update metadata
#'
#' `update_metadata` update metadata from previous queries.
#' @inheritParams template_params
#' @param path Directory path where the .csv file will be saved. Only
#' applicable for [query_macaulay()] query results. By default it
#' is saved into the current working directory (`"."`).
#' @param api_key Character string referring to the key assigned by
#' Xeno-Canto as authorization for searches. Get yours at
#' [https://xeno-canto.org/account](https://xeno-canto.org/account). Only
#' needed if the input metadata comes from [query_xenocanto()].
#' @param dates Optional numeric vector with years to split the search. If
#' provided, the function will perform separate queries for each date range
#' (between consecutive date values) and combine the results. Useful for
#' queries that return large number of results (i.e. > 10000 results limit).
#' For example, to search for the species between 2010 to 2020 and between
#' 2021 to 2025 use `dates = c(2010, 2020, 2025)`. If years contain
#' decimals searches will be split by months within years as well. Only
#' needed if the input metadata comes from [query_macaulay()].
#' @export
#' @name update_metadata
#' @return returns a data frame similar to the input 'metadata' with new
#' data appended.
#' @details This function updates the metadata from a previous query to
#' add entries found in the source repository. **All observations must belong
#' to the same repository** (but see examples for code to update metadata from
#' multiple repositories). The function adds the column `new_entry` which
#' labels those entries that are new (i.e., not present in the input metadata).
#' The input data frame must have been obtained from any of the query
#' functions with the argument `raw_data = FALSE`. The function uses the same
#' query species and format as in the original query. If no new entries are
#' found, the function returns the original metadata and prints a message. If
#' some old entries are not returned in the new query they are still retained.
#' The function assumes that no new files are added to existing repository
#' entries. The value of `all_data` (an argument common to all query functions)
#' is inferred from the columns present in metadata. If columns beyond the
#' standard output are detected, the function assumes `all_data = TRUE`.
#' Columns added during processing by any `suwo` function ("source",
#' "new_entry", "downloaded_file_name", "download_status", "file_size",
#' "duplicate_group") are ignored to prevent incorrect inference.
#'
#' @examples
#' # query metadata
#' a_gioiosa <- query_gbif(species = "Amanita gioiosa", format =  "image")
#'
#' # run if query didnt fail
#' if (!is.null(a_gioiosa)) {
#'
#' # remove the key with more observations
#' sub_a_gioiosa <-
#' a_gioiosa[a_gioiosa$key != names(which.max(table(a_gioiosa$key))), ]
#'
#' # update
#' up_a_gioiosa <- update_metadata(metadata = sub_a_gioiosa)
#'
#' # check number of rows is the same (e.g. it has been updated)
#' nrow(up_a_gioiosa) == nrow(a_gioiosa)
#'
#' # example multi repository update
#' \donttest{
#' a_orientigemmata <- query_inaturalist(species = "Amanita orientigemmata",
#' format =  "image")
#'
#' #remove the key with more observations
#' sub_a_orientigemmata <-
#' a_orientigemmata[a_orientigemmata$key !=
#' names(which.max(table(a_orientigemmata$key))), ]
#'
#' # merge both metadata
#' sub_amanitas <- merge_metadata(sub_a_gioiosa, sub_a_orientigemmata)
#'
#' # split by repository and update separately
#' up_amanitas_list <-
#' lapply(split(sub_amanitas, sub_amanitas$repository), update_metadata)
#'
#' # merge updated metadata
#' up_amanitas <- do.call(merge_metadata, up_amanitas_list)
#'
#'  # check number of rows is the same (e.g. it has been updated)
#'  nrow(up_amanitas) == nrow(a_gioiosa) + nrow(a_orientigemmata)
#' }
#' }
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
update_metadata <-
  function(
    metadata,
    path = ".",
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    api_key = NULL,
    dates = NULL
  ) {
    ##  argument checking
    check_results <- .check_arguments(
      fun = "query_gbif",
      args = list(
        metadata = metadata,
        path = path,
        cores = cores,
        pb = pb,
        verbose = verbose,
        api_key = api_key,
        dates = dates
      )
    )

    # report errors
    .report_assertions(check_results)

    if (length(unique(metadata$repository)) > 1) {
      cli::cli_abort(
        "All observations must belong to the same repository. ",
        "Please provide a single repository query result to update_metadata()."
      )
    }

    if (length(unique(metadata$species)) > 1) {
      cli::cli_abort(
        "All observations must belong to the same species. ",
        "Please provide a single repository query result to update_metadata()."
      )
    }

    #Set query species and format for new query search
    query_species <- metadata$species[1]
    query_format <- metadata$format[1]
    # if more than basic columns are present, assume user wants all columns
    # this column names are those added by this function and download_media
    ignore_cols <-
      c(
        "source",
        "new_entry",
        "downloaded_file_name",
        "download_status",
        "duplicate_group",
        "file_size"
      )

    all_data <-
      length(setdiff(
        names(metadata),
        c(.format_query_output(only_basic_columns = TRUE), ignore_cols)
      )) >
        0

    if (metadata$repository[1] == "GBIF") {
      query_output_new <- query_gbif(
        species = query_species,
        format = query_format,
        all_data = all_data,
        cores = cores,
        verbose = verbose,
        pb = pb
      )
    }

    if (metadata$repository[1] == "iNaturalist") {
      query_output_new <- query_inaturalist(
        species = query_species,
        format = query_format,
        all_data = all_data,
        cores = cores,
        verbose = verbose,
        pb = pb
      )
    }
    if (metadata$repository[1] == "Macaulay Library") {
      query_output_new <- query_macaulay(
        species = query_species,
        format = query_format,
        all_data = all_data,
        path = path,
        dates = dates,
        verbose = verbose
      )
    }

    if (metadata$repository[1] == "Xeno-Canto") {
      # Check for API key
      .check_api_key(api_key)

      query_output_new <- query_xenocanto(
        species = query_species,
        cores = cores,
        all_data = all_data,
        verbose = verbose,
        pb = pb,
        api_key = api_key
      )
    }
    if (metadata$repository[1] == "WikiAves") {
      query_output_new <- query_wikiaves(
        species = query_species,
        format = query_format,
        all_data = all_data,
        cores = cores,
        verbose = verbose,
        pb = pb
      )
    }

    # stop gracefully if query returned NULL
    if (is.null(query_output_new)) {
      if (verbose) {
        .message("No new entries found", "failure", suffix = "\n")
      }
      return(invisible(NULL))
    }

    # Find duplicates
    query_output_new <- query_output_new[
      !query_output_new$key %in% metadata$key,
    ]

    if (nrow(query_output_new) == 0) {
      if (verbose) {
        .message("No new entries found", "failure", suffix = "\n")
      }
      return(metadata)
    }

    query_output_df <- merge_metadata(metadata, query_output_new)

    # remove merge_metadata added column
    query_output_df$source <- NULL

    # tag new entries
    query_output_df$new_entry <- ifelse(
      query_output_df$key %in% metadata$key,
      FALSE,
      TRUE
    )

    sum_new <- sum(query_output_df$new_entry)

    if (verbose) {
      if (sum_new > 0) {
        .message(
          text = paste("\n", sum_new, "new entries found"),
          "success",
          suffix = "\n"
        )
      }
    }

    return(query_output_df)
  }
