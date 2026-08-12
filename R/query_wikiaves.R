#' Query WikiAves for bird media metadata
#'
#' @description
#' Searches WikiAves (\url{https://www.wikiaves.com.br}) for observations of
#' a given species and returns metadata for matching image or sound
#' recordings, including download links, locality, author, and verification
#' status.
#'
#' WikiAves sits behind Cloudflare's bot protection, so this function
#' requires valid authentication cookies (and a matching browser user
#' agent) to be passed via the \code{cookies} argument; see \strong{Details}
#' below and \code{\link{access_wikiaves}}.
#'
#' @param species Character string giving the scientific name of the
#'   species to search for (e.g. \code{"Procnias averano"}). Defaults to
#'   \code{getOption("suwo_species")}.
#' @param format Character string, either \code{"image"} or \code{"sound"},
#'   indicating which type of media to query. Defaults to
#'   \code{getOption("suwo_format", c("image", "sound"))}.
#' @param cores Numeric. Number of cores to use for parallel processing of
#'   paginated results. Defaults to \code{getOption("suwo_cores", 1)}.
#' @param pb Logical. Whether to show a progress bar. Defaults to
#'   \code{getOption("suwo_pb", TRUE)}.
#' @param verbose Logical. Whether to print progress and error messages.
#'   Defaults to \code{getOption("suwo_verbose", TRUE)}.
#' @param all_data Logical. Whether to return all available columns rather
#'   than the standard \pkg{suwo} output columns. Defaults to
#'   \code{getOption("suwo_all_data", FALSE)}.
#' @param raw_data Logical. Whether to return the raw, unformatted query
#'   output instead of the standardized \pkg{suwo} output. Defaults to
#'   \code{getOption("suwo_raw_data", FALSE)}.
#' @param cookies Single character string of WikiAves authentication
#'   credentials, used to get past Cloudflare's bot protection on every
#'   request this function makes. This is exactly the JSON string returned
#'   by \code{\link{access_wikiaves}}, encoding \code{cf_clearance},
#'   \code{PHPSESSID}, and \code{browser_ua} (the exact user agent of the
#'   browser session that obtained the cookies; \code{cf_clearance} is tied
#'   to that user agent, so a mismatched or default user agent will cause
#'   requests to be rejected even with otherwise-valid cookies).
#'   \code{query_wikiaves} parses this string internally before making
#'   requests.
#'
#'   Because it is a single string, it can be stored and reused the same
#'   way other \pkg{suwo} API credentials are, e.g.:
#'   \preformatted{
#'   cookies_live <- access_wikiaves()
#'   Sys.setenv(wikiaves_cookies = cookies_live)
#'   query_wikiaves(species = "Procnias averano", cookies = cookies_live)
#'   }
#'   Defaults to \code{Sys.getenv("wikiaves_cookies")}, which will be empty
#'   unless that environment variable has been set as shown above.
#'
#' @details
#' \strong{Do I need to call \code{\link{access_wikiaves}} before every
#' call to \code{query_wikiaves}?} Not every call, but the credentials do
#' expire. \code{\link{access_wikiaves}} only needs to be run once per
#' Cloudflare session -- the resulting \code{cf_clearance} cookie is
#' typically valid for roughly an hour (Cloudflare does not publish an
#' exact figure and the duration can vary), after which requests made with
#' it will start failing again with an HTTP 403. If \code{query_wikiaves}
#' is scraping many pages for a species with a large number of recordings,
#' or if some time has passed since \code{cookies_live} was generated, it
#' is safest to call \code{\link{access_wikiaves}} again to obtain a fresh
#' string before retrying. \code{\link{access_wikiaves}} reuses the same
#' Chrome profile directory by default, so repeat calls are usually fast
#' and do not require solving the Cloudflare challenge by hand each time.
#'
#' @return
#' A data frame of WikiAves observations matching \code{species} and
#' \code{format}, with standardized \pkg{suwo} columns (or all available
#' columns if \code{all_data = TRUE}, or the raw query output if
#' \code{raw_data = TRUE}). Returns \code{invisible(NULL)} if the species is
#' not found, no matching records exist, or the query otherwise fails.
#'
#' @seealso \code{\link{access_wikiaves}}, which generates the
#'   \code{cookies} argument this function requires.
#'
#' @examples
#' if (interactive()) {
#' # Obtain fresh authentication cookies (only needs to be re-run once the
#' # cookies expire, roughly every hour):
#' cookies_live <- access_wikiaves()
#'
#' # Query sound recordings for a species:
#' result <- query_wikiaves(
#'   species = "Procnias averano",
#'   format = "sound",
#'   cookies = cookies_live
#' )
#'
#' # Query image records instead:
#' result_images <- query_wikiaves(
#'   species = "Procnias averano",
#'   format = "image",
#'   cookies = cookies_live
#' )
#' }
#'
#' @export
query_wikiaves <-
  function(
    species = getOption("suwo_species"),
    format = getOption("suwo_format", c("image", "sound")),
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    all_data = getOption("suwo_all_data", FALSE),
    raw_data = getOption("suwo_raw_data", FALSE),
    cookies = Sys.getenv("wikiaves_cookies")
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
    # if (!.checkconnection(verb = verbose, service = "wikiaves")) {
    #   return(invisible(NULL))
    # }

    # assign a value to format
    format <- rlang::arg_match(format, values = c("image", "sound"))

    wiki_format <- switch(format, sound = "s", image = "f")

    # ---- parse the single JSON cookies string produced by --------------
    # access_wikiaves() into a named vector, then split the browser UA out
    # of it. JSON (rather than a hand-rolled "key=value;..." format) is
    # used because browser_ua commonly contains literal semicolons
    # (e.g. "Mozilla/5.0 (X11; Linux x86_64) ..."), which would otherwise
    # collide with a semicolon-delimited pair separator.
    .parse_wikiaves_cookies <- function(cookies) {
      if (is.null(cookies) || !nzchar(cookies)) {
        return(character(0))
      }

      parsed <- try(
        jsonlite::fromJSON(cookies),
        silent = TRUE
      )

      if (.is_error(parsed) || !is.list(parsed) && !is.character(parsed)) {
        rlang::abort(
          paste(
            "Could not parse `cookies`. Expected the JSON string returned",
            "by access_wikiaves(), e.g. via",
            "Sys.setenv(wikiaves_cookies = access_wikiaves())."
          ),
          class = "wikiaves_cookies_parse_error"
        )
      }

      unlist(parsed)
    }

    cookies_parsed <- .parse_wikiaves_cookies(cookies)

    browser_ua <- cookies_parsed[["browser_ua"]]
    cookie_vec <- cookies_parsed[names(cookies_parsed) != "browser_ua"]

    # helper to build a request with the shared UA / headers / cookies,
    # matching probe_wikiaves_debug
    .wikiaves_request <- function(url) {
      req <- httr2::request(url)
      req <- httr2::req_user_agent(req, browser_ua)
      req <- httr2::req_headers(
        req,
        Accept = "application/json, text/plain, */*",
        Referer = "https://www.wikiaves.com.br/",
        `Accept-Language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7"
      )

      if (length(cookie_vec) > 0) {
        req <- do.call(
          httr2::req_cookies_set,
          c(list(req), as.list(cookie_vec))
        )
      }

      req
    }
    # -------------------------------------------------------------------------

    # initialize search with user agent
    request_obj <- .wikiaves_request(
      "https://www.wikiaves.com.br/getTaxonsJSON.php"
    )

    request_obj <- httr2::req_url_query(
      request_obj,
      term = species
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
        request_obj <- .wikiaves_request(
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

        request_obj <- .wikiaves_request(
          "https://www.wikiaves.com.br/getRegistrosJSON.php"
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
    # query_output_df$format <- format
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
        # "species_id" = "species_code",
        "autor" = "user_name"
      ),
      all_data = all_data,
      format = format,
      raw_data = raw_data
    )

    return(query_output_df)
  }
