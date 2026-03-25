#' Access 'iNaturalist' media file metadata
#'
#' `query_inaturalist` searches for metadata from
#' [iNaturalist](https://www.inaturalist.org/).
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Currently 'image' and 'sound' are available. Can be set globally for
#' the current R session via the "suwo_format" option
#' (e.g. `options(suwo_format = "image")`). Required.
#' @param all_data Logical argument that determines if all data available
#' from database is shown in the results of search. Default is `TRUE`.
#' @param identified Logical argument to define if search results are
#' categorized as identified by inaturalist.
#' @param verifiable Logical argument to define if search results are
#' categorized as verifiable by inaturalist.
#' @export
#' @name query_inaturalist
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for species observation info in the
#' open-access
#' online repository [iNaturalist](https://www.inaturalist.org/).
#' iNaturalist is a free, crowdsourced online platform for nature enthusiasts
#' to document and identify plants, animals, fungi, and other organisms in
#' the wild. Note that Inaturalist observations do not include a 'country'
#' field.
#' @examples
#' # search Bleeding Tooth mushroom images
# h_peckii <- query_inaturalist(species = 'Hydnellum peckii', format = "image")
#'
#' @references
#' iNaturalist. Available from https://www.inaturalist.org.
#' (Accessed on 10-02-2026)
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'

query_inaturalist <- function(
  species = getOption("suwo_species"),
  format = getOption("suwo_format", c("image", "sound")),
  cores = getOption("suwo_cores", 1),
  pb = getOption("suwo_pb", TRUE),
  verbose = getOption("suwo_verbose", TRUE),
  all_data = getOption("suwo_all_data", FALSE),
  raw_data = getOption("suwo_raw_data", FALSE),
  identified = FALSE,
  verifiable = FALSE
) {
  ##  argument checking
  check_results <- .check_arguments(
    fun = "query_inaturalist",
    args = list(
      species = species,
      format = format,
      cores = cores,
      pb = pb,
      verbose = verbose,
      all_data = all_data,
      raw_data = raw_data,
      identified = identified,
      verifiable = verifiable
    )
  )

  .report_assertions(check_results)

  format <- rlang::arg_match(format, values = c("image", "sound"))
  inat_format <- if (format == "sound") "sounds" else "photos"

  if (!.checkconnection(verb = verbose, service = "inat")) {
    return(invisible(NULL))
  }

  base_url <- paste0(
    "https://api.inaturalist.org/v1/observations?",
    "per_page=200&",
    "taxon_name=",
    gsub(" ", "%20", species),
    "&",
    inat_format,
    "=true&",
    "identified=",
    identified,
    "&",
    "verifiable=",
    verifiable
  )

  first_query <- tryCatch(
    jsonlite::fromJSON(base_url),
    error = function(e) e
  )

  if (.is_error(first_query) || is.null(first_query$total_results)) {
    if (verbose) {
      .message(text = "Metadata could not be downloaded", as = "failure")
    }
    return(invisible(NULL))
  }

  total_results <- first_query$total_results

  if (!is.numeric(total_results) || total_results == 0) {
    if (verbose) {
      .message(text = "No matching records found", as = "failure")
    }
    return(invisible(NULL))
  }

  if (verbose) {
    .message(n = total_results, as = "success")
  }

  pages <- seq_len(ceiling(total_results / 200))

  query_output_list <- .pbapply_sw(
    X = pages,
    cl = cores,
    pbar = pb,
    FUN = function(x, Y = pages) {
      page <- Y[x]

      query_output <- tryCatch(
        jsonlite::fromJSON(paste0(base_url, "&page=", page)),
        error = function(e) NULL
      )

      if (
        is.null(query_output) ||
          is.null(query_output$results) ||
          !NROW(query_output$results)
      ) {
        return(NULL)
      }

      res <- query_output$results

      out <- lapply(seq_len(NROW(res)), function(u) {
        row <- as.data.frame(res[u, ], stringsAsFactors = FALSE)

        media <- if (format == "sound") row$sounds else row$photos
        if (is.null(media) || !length(media)) {
          return(NULL)
        }

        media_df <- tryCatch(
          do.call(rbind, media),
          error = function(e) NULL
        )
        if (is.null(media_df)) {
          return(NULL)
        }

        media_df <- media_df[!vapply(media_df, is.list, logical(1))]
        media_df <- data.frame(media_df, stringsAsFactors = FALSE)

        row <- row[!vapply(row, is.list, logical(1))]
        row_df <- data.frame(t(unlist(row)), stringsAsFactors = FALSE)

        X_df <- cbind(row_df, media_df)

        if (format == "image" && "url" %in% names(X_df)) {
          X_df$url <- gsub("square", "original", X_df$url)
        }

        X_df
      })

      .merge_data_frames(Filter(Negate(is.null), out))
    }
  )

  query_output_list <- Filter(Negate(is.null), query_output_list)

  if (!length(query_output_list)) {
    if (verbose) {
      .message(text = "Metadata could not be downloaded", as = "failure")
    }
    return(invisible(NULL))
  }

  query_output_df <- .merge_data_frames(query_output_list)

  if (!NROW(query_output_df)) {
    return(invisible(NULL))
  }

  if ("location" %in% names(query_output_df)) {
    split_location <- strsplit(as.character(query_output_df$location), ",")
    query_output_df$latitude <- vapply(split_location, `[`, "", 1)
    query_output_df$longitude <- vapply(split_location, `[`, "", 2)
  } else {
    query_output_df$latitude <- NA
    query_output_df$longitude <- NA
  }

  id_idx <- which(names(query_output_df) == "id")
  if (length(id_idx)) {
    names(query_output_df)[id_idx[1]] <- "key"
  }

  query_output_df$key <- as.character(query_output_df$key)
  query_output_df$species <- species

  url_col <- grep("url", names(query_output_df), value = TRUE)
  if (length(url_col)) {
    query_output_df$file_extension <- sub(
      ".*\\.",
      "",
      sub("\\?.*", "", query_output_df[[url_col[1]]])
    )
  } else {
    query_output_df$file_extension <- NA
  }

  query_output_df$country <- NA

  if ("time_observed_at" %in% names(query_output_df)) {
    query_output_df$date <- substr(query_output_df$time_observed_at, 1, 10)
  } else {
    query_output_df$date <- NA
  }

  if ("attribution" %in% names(query_output_df)) {
    query_output_df$user_name <- vapply(
      query_output_df$attribution,
      function(x) {
        if (is.na(x)) {
          return(NA_character_)
        }

        # remove leading copyright markers
        x <- gsub("^\\s*\\(c\\)\\s*", "", x, ignore.case = TRUE)

        # stop at comma or license parentheses
        x <- strsplit(x, ",|\\(")[[1]][1]

        trimws(x)
      },
      FUN.VALUE = character(1)
    )
  } else {
    query_output_df$user_name <- NA
  }

  query_output_df$observation_url <- paste0(
    "https://www.inaturalist.org/observations/",
    query_output_df$key
  )


  query_output_df <- .format_query_output(
    X = query_output_df,
    call = base::match.call(),
    column_names = c(
      "location" = "locality",
      "time_observed_at" = "time",
      "url" = "file_url"
    ),
    all_data = all_data,
    format = format,
    raw_data = raw_data
  )

  query_output_df[!is.na(query_output_df$file_url), ]
}
