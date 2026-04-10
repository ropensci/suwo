#' Access 'gbif' media file metadata
#'
#' `query_gbif` searches for metadata from [GBIF](https://www.gbif.org/).
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Options are 'sound', 'image', 'video' and 'interactive resource'.
#' Can be set globally for the current R session via the "suwo_format"
#' option (e.g. `options(suwo_format = "image")`). Required.
#' @param dataset The name of a specific dataset in which to focus the query
#' (by default it searchs across all available datasets).
#' Users can check available dataset names by downloading this csv file
#'  \url{https://api.gbif.org/v1/dataset/search/export?format=CSV&}.
#'  See \url{https://www.gbif.org/dataset/search?q=} for more details.
#' @export
#' @name query_gbif
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for species observation info in the
#' open-access
#' online repository [GBIF](https://www.gbif.org/).
#' GBIF (the Global Biodiversity Information Facility) is an international
#' network and data infrastructure funded by the world's governments and
#' aimed at providing open access to data about all types of life on Earth.
#' Note that some of the records returned by this function could be duplicates
#' of records returned by other suwo functions
#' (e.g., [query_inaturalist()]).
#' @seealso [query_inaturalist()]
#' @examples
#' # search dink frog sound files
# d_diastema <- query_gbif(species = "Diasporus diastema", format = "sound")
#'
#' @references
#' GBIF.org (2024), GBIF Home Page. Available from: https://www.gbif.org/
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_gbif <-
  function(
    species = getOption("suwo_species"),
    format = getOption(
      "suwo_format",
      c("image", "sound", "video", "interactive resource")
    ),
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    dataset = NULL,
    all_data = getOption("suwo_all_data", FALSE),
    raw_data = getOption("suwo_raw_data", FALSE)
  ) {
    ##  argument checking
    check_results <- .check_arguments(
      fun = "query_gbif",
      args = list(
        species = species,
        format = format,
        cores = cores,
        pb = pb,
        verbose = verbose,
        dataset = dataset,
        all_data = all_data,
        raw_data = raw_data
      )
    )

    .report_assertions(check_results)

    ##  connection
    if (!.checkconnection(verb = verbose, service = "gbif")) {
      return(invisible(NULL))
    }

    ##  format
    format <- rlang::arg_match(
      format,
      values = c("image", "sound", "video", "interactive resource")
    )

    gbif_format <- switch(
      format,
      sound = "Sound",
      image = "StillImage",
      video = "MovingImage",
      `interactive resource` = "InteractiveResource"
    )

    ## base request
    base_request <- httr2::request("https://api.gbif.org/v1/occurrence/search")

    base_request <-
      httr2::req_user_agent(
        base_request,
        "suwo (https://github.com/ropensci/suwo)"
      )

    base_request <- httr2::req_url_query(
      base_request,
      limit = 300,
      scientificName = species,
      media_type = gbif_format,
      datasetKey = dataset
    )

    base_request <- httr2::req_error(
      base_request,
      is_error = function(resp) {
        FALSE
      }
    )

    ##  first request (metadata)
    response <- try(httr2::req_perform(base_request), silent = TRUE)

    if (.is_error(response) || httr2::resp_is_error(response)) {
      if (verbose) {
        .message("Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    base.srch.pth <- httr2::resp_body_json(response, simplifyVector = TRUE)

    if (base.srch.pth$count == 0) {
      if (verbose) {
        .message("No matching records found", as = "failure")
      }
      return(invisible(NULL))
    }

    if (verbose) {
      .message(n = base.srch.pth$count, as = "success")
    }

    ##  pagination
    offsets <- (seq_len(ceiling(
      base.srch.pth$count / base.srch.pth$limit
    )) -
      1) *
      base.srch.pth$limit

    ##  paged download
    query_output_list <- .pbapply_sw(
      X = seq_along(offsets),
      cl = cores,
      pbar = pb,
      FUN = function(x, Y = offsets) {
        i <- Y[x]

        resp <-
          try(
            httr2::req_perform(httr2::req_url_query(base_request, offset = i)),
            silent = TRUE
          )

        if (.is_error(resp) || httr2::resp_is_error(resp)) {
          return(resp)
        }

        query_output <- try(
          httr2::resp_body_json(resp, simplifyVector = TRUE),
          silent = TRUE
        )

        if (.is_error(query_output)) {
          return(query_output)
        }

        if (
          is.null(query_output$results) ||
            nrow(query_output$results) == 0
        ) {
          return(NULL)
        }

        query_output$results <- lapply(
          seq_len(nrow(query_output$results)),
          function(u) {
            x <- query_output$results[u, ]

            media_df <- do.call(rbind, x$media)
            media_df <- media_df[media_df$type == gbif_format, ]

            names(media_df)[names(media_df) == "identifier"] <- "URL"
            names(media_df) <- paste0("media-", names(media_df))

            x <- x[!vapply(x, is.list, logical(1))]
            X_df <- data.frame(t(unlist(x)))
            cbind(X_df, media_df)
          }
        )

        output_df <- .merge_data_frames(query_output$results)
        output_df$page <- i
        output_df
      }
    )

    ##  error handling
    if (any(vapply(query_output_list, .is_error, logical(1)))) {
      if (verbose) {
        .message("Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    query_output_df <- .merge_data_frames(query_output_list)

    if (is.null(query_output_df)) {
      return(query_output_df)
    }

    ##  post-processing
    query_output_df$species <-
      vapply(
        strsplit(query_output_df$species, " "),
        function(x) {
          paste(x[1], x[2])
        },
        character(1)
      )

    query_output_df$gbifid <- query_output_df$scientificName <- NULL

    query_output_df$observation_url <-
      paste0("https://www.gbif.org/occurrence/", query_output_df$key)

    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "media-URL" = "file_url",
        "eventDate" = "date",
        "decimalLatitude" = "latitude",
        "decimalLongitude" = "longitude",
        "specificepithet" = "specific_epithet",
        "recordedby" = "recordist",
        "stateprovince" = "state_province",
        "specieskey" = "species_code",
        "genuskey" = "genus_code",
        "kingdomkey" = "kingdom_code",
        "phylumkey" = "phylum_code",
        "classkey" = "class_code",
        "orderkey" = "order_code",
        "familykey" = "family_key",
        "fieldnotes" = "comments",
        "eventtime" = "time",
        "media-creator" = "user_name",
        "media-format" = "file_extension"
      ),
      all_data = all_data,
      format = format,
      raw_data = raw_data
    )

    query_output_df
  }
