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
#' add entries found in the source repository. All observations must belong
#' to the same repository. The function adds the column `new_entry` which
#' labels those entries that are new (i.e., not present in the input metadata).
#' The input data frame must have been obtained from any of the query
#' functions with the argument `raw_data = FALSE`. The function uses the same
#' query species and format as in the original query. If no new entries are
#' found, the function returns the original metadata and prints a message. If
#' some old entries are not returned in the new query they are still retained.
#' The function assumes that no new files are added to existing repository
#' entries.
#' @examples
#' # query metadata
#' a_gioiosa <- query_gbif(species = "Amanita gioiosa", format =  "image")
#'
#' # run if query didnt fail
#'  if (!is.null(a_gioiosa)) {
#' # remove last 3 rows to test update_metadata
#' sub_a_gioiosa <- a_gioiosa[1:(nrow(a_gioiosa)- 3), ]
#'
#' # update
#' up_a_gioiosa <- update_metadata(metadata = sub_a_gioiosa)
#'
#' # check number of rows is the same
#' # nrow(up_a_gioiosa) == nrow(a_gioiosa)
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
    all_data <-
      length(setdiff(
        names(metadata),
        .format_query_output(only_basic_columns = TRUE)
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
