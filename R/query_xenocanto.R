#' Access 'Xeno-Canto' recording metadata
#'
#' `query_xenocanto` searches for metadata from
#' [Xeno-Canto](https://www.xeno-canto.org/).
#' @inheritParams template_params
#' @param species Character string with the scientific name of a species in
#' the format: "Genus epithet". Required. Can be set globally for the current
#' R session via the "suwo_species" option (e.g.
#' `options(suwo_species = "Hypsiboas rufitelus")`). Alternatively, a character
#' string containing additional tags that follows the Xeno-Canto advanced query
#' syntax can be provided. Tags are of the form 'tag:searchterm'. For
#' instance, `'type:"song"'` will search for recordings where the sound
#' type contains 'song'. Multiple tags can be provided
#' (e.g., `'"cnt:"belize" type:"song"'`).
#'  See examples down below and check
#'  [Xeno-Canto's search help](https://www.xeno-canto.org/help/search)
#'  for a full description.
#' @param api_key Character string refering to the key assigned by Xeno-Canto
#' as authorization for searches. Get yours at
#' [https://xeno-canto.org/account](https://xeno-canto.org/account).
#' Required. Avoid setting your API key directly in the function call to
#' prevent exposing it in your code. Instead, set it as an environment variable
#' (e.g., in your .Renviron file using
#' `Sys.setenv(xc_api_key = "your_key_here")`) named 'xc_api_key',
#' so it can be accessed
#' securely using `Sys.getenv("xc_api_key")`.
#' @export
#' @name query_xenocanto
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries metadata for animal sound recordings in
#' the open-access
#'  online repository [Xeno-Canto](https://www.xeno-canto.org/).
#'  [Xeno-Canto](https://www.xeno-canto.org/) hosts sound recordings of
#'  birds, frogs, non-marine mammals and grasshoppers. Complex queries can be
#'  constructed using the [Xeno-Canto](https://www.xeno-canto.org/)
#'  advanced query syntax (see examples).
#' @seealso [query_gbif()], [query_wikiaves()],
#' [query_inaturalist()]
#' @examples
#' if (interactive()){
#' # An API key is required. Get yours at https://xeno-canto.org/account.
#' # run this in the console but dont save it in script
#' Sys.setenv(xc_api_key = "YOUR_API_KEY_HERE")
#'
#' # Simple search for a species
#' p_anth <- query_xenocanto(species = "Phaethornis anthophilus")
#'
#' # Search for same species and add specify country
#' p_anth_cr <- query_xenocanto(
#' species = 'sp:"Phaethornis anthophilus" cnt:"Panama"',
#' raw_data = TRUE)
#'
#' # Search for female songs of a species
#' femsong <-  query_xenocanto(
#' species = 'sp:"Thryothorus ludovicianus" type:"song" type:"female"')
#' }
#'
#' @references
#' Planqué, Bob, & Willem-Pier Vellinga. 2008. Xeno-canto: a 21st-century way
#' to appreciate Neotropical bird song. Neotrop. Birding 3: 17-23.
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_xenocanto <-
  function(
    species = getOption("suwo_species"),
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    all_data = getOption("suwo_all_data", FALSE),
    raw_data = getOption("suwo_raw_data", FALSE),
    api_key = Sys.getenv("xc_api_key")
  ) {
    ##  argument checking
    check_results <- .check_arguments(
      fun = "query_xenocanto",
      args = list(
        species = species,
        cores = cores,
        pb = pb,
        verbose = verbose,
        all_data = all_data,
        raw_data = raw_data
      )
    )

    # Check for API key
    if (is.null(api_key) || !nzchar(api_key)) {
      cli::cli_abort(
        paste(
          "An API key is required for Xeno-Canto API v3.",
          "Get yours at https://xeno-canto.org/account."
        )
      )
    }

    # build query from tags
    if (!grepl(":", species)) {
      species_name <- ifelse(
        grepl("\\s", species),
        paste0('"', species, '"'),
        species
      )
      query_str <- paste0("sp:", species_name)
    } else {
      query_str <- paste(species, collapse = " ")
    }

    query_str <- utils::URLencode(query_str, reserved = TRUE)

    if (verbose) {
      .message("Obtaining metadata:", as = "message")
    }

    # API request
    query <- try(
      jsonlite::fromJSON(
        paste0(
          "https://www.xeno-canto.org/api/3/recordings?query=",
          query_str,
          "&key=",
          api_key
        )
      ),
      silent = TRUE
    )

    if (.is_error(query)) {
      if (verbose) {
        .message(
          text = "Metadata could not be downloaded (is your API key valid?)",
          as = "failure",
          suffix = "\n"
        )
      }
      return(invisible(NULL))
    }

    if (as.numeric(query$numRecordings) == 0) {
      if (verbose) {
        .message(
          text = "No matching records found",
          as = "failure",
          suffix = "\n"
        )
      }
      return(invisible(NULL))
    }

    query_output_list <- .pbapply_sw(
      pbar = pb,
      X = seq_len(ceiling(as.numeric(query$numRecordings) / 100)),
      cl = cores,
      FUN = function(
        x,
        Y = seq_len(ceiling(as.numeric(query$numRecordings) / 100))
      ) {
        y <- Y[x]

        query_output <- try(
          jsonlite::fromJSON(
            paste0(
              "https://www.xeno-canto.org/api/3/recordings?query=",
              query_str,
              "&page=",
              y,
              "&key=",
              api_key
            )
          ),
          silent = TRUE
        )

        if (.is_error(query_output)) {
          return(query_output)
        }

        query_output$recordings$also <-
          vapply(
            query_output$recordings$also,
            paste,
            collapse = "-",
            FUN.VALUE = character(1)
          )

        sono_df <- as.data.frame(query_output$recordings$sono)
        names(sono_df) <- paste("sonogram", names(sono_df), sep = "_")

        osci_df <- as.data.frame(query_output$recordings$osci)
        names(osci_df) <- paste("oscillogram", names(osci_df), sep = "_")

        query_output$recordings$sono <- query_output$recordings$osci <- NULL

        cbind(query_output$recordings, sono_df, osci_df)
      }
    )

    if (any(vapply(query_output_list, .is_error, logical(1)))) {
      if (verbose) {
        .message(
          text = "Metadata could not be downloaded",
          as = "failure",
          suffix = "\n"
        )
      }
      return(invisible(NULL))
    }

    query_output_df <- .merge_data_frames(query_output_list)

    if (as.numeric(query$numRecordings) > 0) {
      indx <- vapply(query_output_df, is.factor, logical(1))
      query_output_df[indx] <- lapply(query_output_df[indx], as.character)

      query_output_df$species <-
        paste(query_output_df$gen, query_output_df$sp, sep = " ")

      query_output_df$file_extension <-
        sub(".*\\.", "", query_output_df$`file-name`)

      query_output_df$file <-
        paste0("https://xeno-canto.org/", query_output_df$id, "/download")

      query_output_df$date <-
        gsub("-", "/", query_output_df$date)

      query_output_df$observation_url <- paste0(
        "https://xeno-canto.org/",
        query_output_df$id
      )


      query_output_df <- .format_query_output(
        X = query_output_df,
        call = base::match.call(),
        column_names = c(
          "id" = "key",
          "gen" = "genus",
          "sp" = "specific_epithet",
          "ssp" = "subspecies",
          "en" = "english_name",
          "rec" = "recordist",
          "cnt" = "country",
          "loc" = "locality",
          "lat" = "latitude",
          "lon" = "longitude",
          "alt" = "altitude",
          "type" = "vocalization_type",
          "file" = "file_url",
          "lic" = "license",
          "url" = "url",
          "q" = "quality",
          "grp" = "taxonomic_group",
          "file-name" = "uploaded_file",
          "sono" = "sonogram",
          "also" = "other_species",
          "smp" = "sampling_rate",
          "dvc" = "recorder",
          "mic" = "microphone",
          "uploaded" = "upload_date",
          "rmk" = "comments",
          "animal.seen" = "animal_seen",
          "playback.used" = "playback_used",
          "recordist" = "user_name"
        ),
        all_data = all_data,
        format = "sound",
        raw_data = raw_data
      )

      # normalize user names
      if ("user_name" %in% names(query_output_df)) {
        query_output_df$user_name <- vapply(
          query_output_df$user_name,
          function(x) {
            if (is.na(x)) {
              return(NA_character_)
            }
            x <- gsub("^\\s*\\(c\\)\\s*", "", x, ignore.case = TRUE)
            trimws(strsplit(x, ",|\\(")[[1]][1])
          },
          FUN.VALUE = character(1)
        )
      }

      if (verbose) {
        .message(
          "{n} matching sound file{?s} found",
          as = "success",
          suffix = "\n",
          n = nrow(query_output_df)
        )
      }

      return(droplevels(query_output_df))
    }
  }
