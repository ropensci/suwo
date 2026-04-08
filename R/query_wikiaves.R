#' Access 'WikiAves' media file metadata
#'
#' `query_wikiaves` searches for metadata from
#' [WikiAves](https://www.wikiaves.com.br/).
#' @inheritParams template_params
#' @param format Character vector with the media format to query for.
#' Options are 'image' or 'sound'. Can be set globally for
#' the current R session via the "suwo_format" option
#' (e.g. `options(suwo_format = "image")`). Required.
#' @export
#' @name query_wikiaves
#' @return The function returns a data frame with the metadata of the media
#' files matching the search criteria. If `all_data = TRUE`, all metadata
#' fields (columns) are returned. If `raw_data = TRUE`, the raw data as
#' obtained from the repository is returned (without any formatting).
#' @details This function queries for avian digital media in the open-access
#' online repository [WikiAves](https://www.wikiaves.com.br/) and returns
#' its metadata. WikiAves is a Brazilian online platform and citizen science
#' project that serves as the largest community for birdwatchers in Brazil.
#' It functions as a collaborative, interactive encyclopedia of Brazilian
#' birds, where users contribute georeferenced photographs and sound
#' recordings, which are then used to build a vast database for research
#' and conservation.
#' @examples  \donttest{
#' # search
#' p_nattereri <- query_wikiaves(species = "Phaethornis nattereri",
#'     format = "image")
#' }
#' @references
#' Schubert, Stephanie Caroline, Lilian Tonelli Manica, and André De Camargo
#' Guaraldo. 2019. Revealing the potential of a huge citizen-science platform
#' to study bird migration. Emu-Austral Ornithology 119.4: 364-373.
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_wikiaves <-
  function(
    species = getOption("suwo_species"),
    format = getOption("suwo_format", c("image", "sound")),
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    all_data = getOption("suwo_all_data", FALSE),
    raw_data = getOption("suwo_raw_data", FALSE)
  ) {
    ##  argument checking
    check_results <- .check_arguments(
      fun = "query_wikiaves",
      args = list(
        species = species,
        format = format,
        cores = cores,
        pb = pb,
        verbose = verbose,
        all_data = all_data,
        raw_data = raw_data
      )
    )

    # report errors
    .report_assertions(check_results)

    # Use the unified connection checker
    if (!.checkconnection(verb = verbose, service = "wikiaves")) {
      return(invisible(NULL))
    }

    # assign a value to format
    format <- rlang::arg_match(format, values = c("image", "sound"))

    wiki_format <- switch(format, sound = "s", image = "f")

    # initialize search with user agent
    request_obj <- httr2::request(
      "https://www.wikiaves.com.br/getTaxonsJSON.php"
    )

    request_obj <- httr2::req_url_query(
      request_obj,
      term = species
    )

    request_obj <- httr2::req_user_agent(
      request_obj,
      "suwo (https://github.com/ropensci/suwo)"
    )

    response <- httr2::req_perform(request_obj)

    # check if request succeeded
    if (httr2::resp_status(response) >= 400) {
      if (verbose) {
        .message(
          text = paste0(
            "Wikiaves query request failed: ",
            httr2::resp_status_desc(response)
          ),
          as = "failure"
        )
      }
      return(invisible(NULL))
    }

    get_ids <- httr2::resp_body_json(
      response,
      check_type = FALSE,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE
    )

    # do exact matching for species name
    if (length(get_ids) > 1) {
      get_ids <- get_ids[
        trimws(tolower(get_ids$label)) == trimws(tolower(species)),
      ]
    }

    if (length(get_ids) == 0) {
      if (verbose) {
        .message("Search species not found", as = "failure")
      }
      return(invisible(NULL))
    }

    get_ids$total_registers <- vapply(
      seq_len(nrow(get_ids)),
      function(u) {
        request_obj <- httr2::request(
          "https://www.wikiaves.com.br/getRegistrosJSON.php"
        )

        request_obj <- httr2::req_url_query(
          request_obj,
          tm = wiki_format,
          t = "s",
          s = get_ids$id[u],
          o = "mp",
          p = 1
        )

        request_obj <-
          httr2::req_user_agent(
            request_obj,
            "suwo (https://github.com/ropensci/suwo)"
          )
        request_obj <- httr2::req_error(request_obj, is_error = function(resp) {
          FALSE
        })

        response <- try(httr2::req_perform(request_obj), silent = TRUE)

        # if fail request return -9999
        if (.is_error(response)) {
          return(-999)
        }

        # check if request succeeded
        if (httr2::resp_is_error(response)) {
          return(-999)
        }

        content <- httr2::resp_body_json(response)
        as.numeric(content$registros$total)
      },
      numeric(1)
    )

    # let user gracefully know error when downloading metadata
    if (
      any(vapply(
        get_ids$total_registers,
        function(x) x == -999,
        FUN.VALUE = logical(1)
      ))
    ) {
      if (verbose) {
        .message(text = "Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    if (sum(get_ids$total_registers) == 0) {
      if (verbose) {
        .message(text = "No matching records found", as = "failure")
      }
      return(invisible(NULL))
    }

    # get number of pages (20 is the default number of registers per page)
    get_ids$pages <- ceiling(get_ids$total_registers / 20)

    # remove those rows with no pages
    # (only needed when many species are returned)
    get_ids <- get_ids[get_ids$pages > 0, ]

    id_by_page_list <- lapply(seq_len(nrow(get_ids)), function(x) {
      X <- get_ids[x, ]
      data.frame(id = X$id, page = 1:X$pages)
    })

    id_by_page_df <- do.call(rbind, id_by_page_list)

    # search recs in wikiaves (results are returned in pages with 500
    # recordings each)
    if (verbose) {
      .message(n = get_ids$total_registers[1], as = "success")
    }

    # loop over pages
    query_output_list <- .pbapply_sw(
      X = seq_len(nrow(id_by_page_df)),
      cl = cores,
      pbar = pb,
      function(x, Y = seq_len(nrow(id_by_page_df))) {
        # set index to get the right offset
        i <- Y[x]

        # wait avoid overloading the server
        # **INTERVARLS < 1s BRAKE THE FUNCTION**
        Sys.sleep(1)

        request_obj <- httr2::request(
          "https://www.wikiaves.com.br/getRegistrosJSON.php"
        )

        request_obj <- httr2::req_user_agent(
          request_obj,
          "suwo (https://github.com/ropensci/suwo)"
        )

        request_obj <- httr2::req_url_query(
          request_obj,
          tm = wiki_format,
          t = "s",
          s = id_by_page_df$id[i],
          o = "mp",
          p = id_by_page_df$page[i]
        )

        # do not auto-throw so we can retry manually
        request_obj <- httr2::req_error(
          request_obj,
          is_error = function(resp) FALSE
        )

        response <- httr2::req_perform(request_obj)

        # retry once if request failed
        if (httr2::resp_is_error(response)) {
          Sys.sleep(1)

          response <- httr2::req_perform(request_obj)
        }

        # if still failing, stop here (no downstream code runs)
        if (httr2::resp_is_error(response)) {
          rlang::abort(
            paste(
              "WikiAves request failed:",
              httr2::resp_status(response),
              httr2::resp_status_desc(response)
            ),
            class = "wikiaves_request_error"
          )
        }

        # parse JSON only if request succeeded
        query_output <-
          httr2::resp_body_json(
            response,
            simplifyVector = TRUE
          )

        # make it a data frame
        output_df <-
          as.data.frame(do.call(
            rbind,
            lapply(
              query_output$registros$itens,
              unlist
            )
          ))

        # fix link
        output_df$link <- gsub("#", "", as.character(output_df$link))

        return(output_df)
      }
    )

    # let user know error when downloading metadata
    if (any(vapply(query_output_list, .is_error, FUN.VALUE = logical(1)))) {
      if (verbose) {
        .message(text = "Metadata could not be downloaded", as = "failure")
      }
      return(invisible(NULL))
    }

    # combine into a single data frame
    query_output_df <- .merge_data_frames(query_output_list)

    # rename rows
    rownames(query_output_df) <- seq_len(nrow(query_output_df))

    # change jpg to mp3 in links
    if (format == "sound") {
      query_output_df$link <-
        gsub(".jpg$", ".mp3", query_output_df$link)
    }

    # remove weird columns
    query_output_df$por <- query_output_df$grande <-
      query_output_df$enviado <- NULL

    # verified?
    query_output_df$is_questionada <-
      as.logical(query_output_df$is_questionada)

    # make NAs observations with no link
    query_output_df$link[grepl("^\\d+$", query_output_df$link)] <- NA

    # add file format
    query_output_df$file_extension <- sub(".*\\.", "", query_output_df$link)

    # add missing basic columns
    query_output_df$format <- format
    query_output_df$country <- "Brazil"

    query_output_df$observation_url <- paste0(
      "https://www.wikiaves.com.br/",
      query_output_df$id
    )

    # rename output columns
    query_output_df <- .format_query_output(
      X = query_output_df,
      call = base::match.call(),
      column_names = c(
        "id" = "key",
        "tipo" = "format",
        "id_usuario" = "user.id",
        "sp.id" = "species_id",
        "sp.nome" = "species",
        "sp.nvt" = "common.name",
        "sp.idwiki" = "repository.id",
        "autor" = "author",
        "perfil" = "user_name",
        "data" = "date",
        "is_questionada" = "verified",
        "local" = "locality",
        "idMunicipio" = "locality.id",
        "coms" = "number_of_comments",
        "likes" = "likes",
        "vis" = "visualizations",
        "link" = "file_url",
        "dura" = "duration",
        "scientific.name" = "species",
        "species_id" = "species_code",
        "author" = "user_name"
      ),
      all_data = all_data,
      format = format,
      raw_data = raw_data
    )

    return(query_output_df)
  }
