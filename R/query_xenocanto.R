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
#' (e.g., `'"cnt:"belize" type:"song"'`). This includes Xeno-Canto's
#' annotation-specific search tags (e.g. `ann_sp`, `ann_type`, `ann_gen`,
#' `ann_frq_low`, `ann_frq_high`; see Xeno-Canto's search help linked
#' below) to filter for recordings that have a matching annotation --
#' note this only filters *which recordings* are returned; the actual
#' annotation-level data for any returned recording is available
#' regardless of whether annotation tags were used in the search (see
#' Details).
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
#'
#' If any of the matching recordings have Xeno-Canto annotations attached
#' (individually annotated sound segments within a recording, distinct from
#' the recording-level metadata above), those are included as a data frame
#' attached to the result via `attr(result, "annotations")` -- see Details.
#' If no matching recordings have any annotations, this attribute is not
#' set (`attr(result, "annotations")` is `NULL`).
#' @details This function queries metadata for animal sound recordings in
#' the open-access
#'  online repository [Xeno-Canto](https://www.xeno-canto.org/).
#'  [Xeno-Canto](https://www.xeno-canto.org/) hosts sound recordings of
#'  birds, frogs, non-marine mammals and grasshoppers. Complex queries can be
#'  constructed using the [Xeno-Canto](https://www.xeno-canto.org/)
#'  advanced query syntax (see examples).
#'
#' **Annotations.** Some Xeno-Canto recordings have one or more annotations
#' attached: individually marked sound segments within the recording, each
#' with their own scientific name, annotator, start/end time (in seconds),
#' frequency range (in Hz), sound type, sex, life stage, and remarks. This
#' is different from (and more granular than) the recording-level metadata
#' returned in the main result -- a single recording can have several
#' annotations, each describing a different segment of that one sound
#' file.
#'
#' Xeno-Canto includes this annotation data in the same API response used
#' to fetch ordinary recording metadata, so retrieving it costs no extra
#' requests and is always extracted (there is no separate argument to turn
#' this on or off). When present, it is available as a data frame via
#' `attr(result, "annotations")`, with one row per annotation; a message
#' reports how many annotations were found and confirms they were added
#' to this attribute. For reference, each annotation row also includes
#' `key` (the Xeno-Canto ID of the recording it belongs to, matching the
#' `key` column of the main result -- use this to join back to the main
#' result if needed), `species` (the species identified in that specific
#' annotated segment -- usually, but not necessarily always, the same as
#' the parent recording's overall species; it can differ for an annotated
#' background call of a different species), plus `file_url` and
#' `observation_url` links to the parent recording.
#'
#' To search specifically for recordings that have annotations matching
#' certain criteria (rather than just inspecting whatever annotations
#' happen to be attached to whatever recordings a search returns), use
#' Xeno-Canto's `ann_*` search tags directly in `species`, e.g.
#' `species = 'ann_sp:"Turdus migratorius" ann_type:"call"'`.
#' @seealso [query_gbif()], [query_wikiaves()],
#' [query_inaturalist()], [download_media()]
#' @examples
#' if (interactive()){
#' # An API key is required. Get yours at https://xeno-canto.org/account.
#' # run this in the console but dont save it in a script
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
#'
#' # Any annotations attached to the returned recordings are available as
#' # an attribute, regardless of the search used:
#' poospiza <- query_xenocanto(species = "Poospiza hispaniolensis")
#' annotations <- attr(poospiza, "annotations")
#' annotations
#'
#' # each annotation row includes `key` and `species` for easy reference
#' # back to the parent recording, e.g. to join with the main result:
#' if (!is.null(annotations)) {
#'   merge(annotations, poospiza, by = "key")
#' }
#'
#' # Searching with an annotation-specific tag filters which recordings
#' # are returned, but does not change how annotation data is retrieved:
#' ann_search <- query_xenocanto(species = 'ann_sp:"Poospiza hispaniolensis"')
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
    .check_api_key(api_key)

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

        # extract annotations (if any) BEFORE dropping/reshaping columns
        # below, since annotation-set lives alongside the other recording
        # fields in the raw response
        page_annotations <- .extract_annotations(query_output$recordings)

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
        query_output$recordings[["annotation-set"]] <- NULL

        list(
          recordings = cbind(query_output$recordings, sono_df, osci_df),
          annotations = page_annotations
        )
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

    # split the per-page list(recordings=, annotations=) results apart
    # before merging each half separately
    recordings_list <- lapply(query_output_list, `[[`, "recordings")
    annotations_list <- lapply(query_output_list, `[[`, "annotations")
    annotations_list <- annotations_list[
      !vapply(annotations_list, is.null, logical(1))
    ]

    query_output_df <- .merge_data_frames(recordings_list)

    annotations_df <- if (length(annotations_list) > 0) {
      .merge_data_frames(annotations_list)
    } else {
      NULL
    }

    if (as.numeric(query$numRecordings) > 0) {
      indx <- vapply(query_output_df, is.factor, logical(1))
      query_output_df[indx] <- lapply(query_output_df[indx], as.character)

      query_output_df$species <-
        paste(query_output_df$gen, query_output_df$sp, sep = " ")

      query_output_df$file_extension <-
        sub(".*\\.", "", query_output_df$`file-name`)

      # predicted file name, replicating download_media()'s naming formula
      # exactly. Unlike a hypothetical per-annotation preview, this is
      # always unambiguous here since each row of this table is already a
      # distinct recording/key (no duplicate-key "-1", "-2", ... suffix
      # scenario applies).
      query_output_df$predicted_file_name <- paste0(
        gsub(" ", "_", query_output_df$species),
        "-XC",
        query_output_df$id,
        ".",
        query_output_df$file_extension
      )

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

      query_output_df <- droplevels(query_output_df)

      if (!is.null(annotations_df)) {
        indx_ann <- vapply(annotations_df, is.factor, logical(1))
        annotations_df[indx_ann] <- lapply(
          annotations_df[indx_ann],
          as.character
        )
        annotations_df <- droplevels(annotations_df)
        attr(query_output_df, "annotations") <- annotations_df
      }

      if (verbose) {
        .message(
          "{n} matching sound file{?s} found",
          as = "success",
          suffix = "\n",
          n = nrow(query_output_df)
        )

        if (!is.null(annotations_df)) {
          .message(
            paste(
              "{n} annotation{?s} found and added to the returned object's",
              "`annotations` attribute (access with",
              "attr(<your_object>, \"annotations\"))"
            ),
            as = "success",
            suffix = "\n",
            n = nrow(annotations_df)
          )
        }
      }

      return(query_output_df)
    }
  }
