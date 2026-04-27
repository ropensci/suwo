#' Download media files from repositories
#'
#' `download_media` downloads media files from online repositories.
#' @inheritParams template_params
#' @param metadata Data frame with the metadata of the media records to be
#' mapped. Typically the output of one of the query functions in this package
#' (e.g. [query_gbif()], [query_inaturalist()], etc.) or metadata formatting
#' functions (e.g. [merge_metadata()], [remove_duplicates()], etc.).
#' @param path Directory path where the output media files will be saved.
#' By default files are saved into the current working directory (`"."`).
#' @param overwrite Logical. If TRUE, existing files (in `"path"`) with
#' the same name will be overwritten. Default is FALSE.
#' @param folder_by Character string with the name of a character or factor
#' column in the metadata data frame. If supplied the function will use the
#' unique values in that column to create subfolders within `"path"` and
#' the files will be downloaded into the corresponding folder. It can be used
#' for instance to save files in subfolders by country or recordist.
#' By default no  subfolders are created and all files are saved in the path
#' provided. Missing values (NAs) are saved in a folder called
#' `paste0("unknown_", folder_by)`. Special characters that are not
#' allowed in folder names will be modified or removed. If any of the folder
#' names already exist in `"path"`, they will be used as is.
#' @return Downloads media files into the supplied directory path
#' (`"path"`) and returns (invisibly) the input data frame with
#' three additional columns: `downloaded_file_name` with the name of
#' the downloaded file (if downloaded or already in the directory),
#' `download_status` with the result of the download process for each
#' file (either "saved", "overwritten", "already there (not downloaded)",
#' or "failed"), and `file_size` with the size of the downloaded file in
#' megabytes (MB). Note that the column `file_size` can be used to further
#' remove duplicated media using [find_duplicates()] and [remove_duplicates()]
#' (e.g. if two files from different repositories
#' have the same size and similar user name).
#' @export
#' @name download_media
#' @details This function will take the output data frame of any of the
#' "query_reponame()" functions and download the associated media files. The
#' function will download all files into a single directory
#' (argument `"path"`). File downloading process can be interrupted and
#' resume later as long as the working directory is the same. Users only need
#' to rerun the same function call. By default only the missing files will be
#' downloaded when resuming. Can also be used on a updated query output
#' (see [update_metadata()]) to add the new media files to the
#' existing media pool.
#' @seealso [query_gbif()], [query_macaulay()]
#' @examples  \donttest{
#'   a_zambiana <- query_inaturalist(species = "Amanita zambiana",
#'   format = "image")
#'
#' # run if query didnt fail
#'  if (!is.null(a_zambiana)) {
#'   # donwload the first to files
#'   phae_anth_downl <- download_media(metadata = a_zambiana[1:2, ],
#'   path = tempdir())
#' }
#'}
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

download_media <-
  function(
    metadata,
    path = ".",
    cores = getOption("suwo_cores", 1),
    pb = getOption("suwo_pb", TRUE),
    verbose = getOption("suwo_verbose", TRUE),
    overwrite = FALSE,
    folder_by = NULL
  ) {
    # check arguments
    check_results <- .check_arguments(
      fun = "download_media",
      args = list(
        metadata = metadata,
        path = path,
        cores = cores,
        pb = pb,
        verbose = verbose,
        overwrite = overwrite,
        folder_by = folder_by
      )
    )

    # report errors
    .report_assertions(check_results)

    # Use the unified connection checker
    if (!.checkconnection(verb = verbose, service = NULL)) {
      return(invisible(NULL))
    }

    repo_short <- vapply(
      metadata$repository,
      function(x) {
        switch(
          x,
          `Xeno-Canto` = "XC",
          Observation = "OBS",
          GBIF = "GBIF",
          WikiAves = "WA",
          iNaturalist = "INAT",
          `Macaulay Library` = "ML"
        )
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

    # rename if any duplicated names
    metadata$non_dup_key <- unlist(lapply(unique(metadata$key), function(x) {
      on <- metadata$key[metadata$key == x]
      if (length(on) > 1) {
        paste0(on, "-", seq_along(on))
      } else {
        x
      }
    }))

    # create file name
    metadata$downloaded_file_name <-
      paste0(
        gsub(pattern = " ", "_", x = metadata$species),
        "-",
        repo_short,
        metadata$non_dup_key,
        ".",
        metadata$file_extension
      )

    # set clusters for windows OS
    if (verbose) {
      write(file = "", x = "Downloading media files:")
    }

    metadata$download_status <-
      unlist(.pbapply_sw(
        pbar = pb,
        X = seq_len(nrow(metadata)),
        cl = cores,
        FUN = function(x) {
          .download(metadata, x, path, overwrite, folder_by)
        }
      ))

    # add folder to file name
    if (!is.null(folder_by)) {
      metadata$downloaded_file_name <-
        file.path(metadata[, folder_by], metadata$downloaded_file_name)
    }

    # report results
    if (length(unique(metadata$download_status)) > 1) {
      report_message <- c()
    }

    # report failed files
    if (any(metadata$download_status == "failed")) {
      if (sum(metadata$download_status == "failed") == nrow(metadata)) {
        report_message <- .message(
          "All files failed to download",
          as = "failure",
          suffix = "\n"
        )
      } else {
        unified_message <- paste(
          "{sum(metadata$download_status == 'failed')}",
          "file{?s} failed to download"
        )
        report_message <- c(
          report_message,
          "x" = paste0(cli::pluralize(unified_message))
        )
      }
      # remove file name from "downloaded_file_name"
      metadata$downloaded_file_name[metadata$download_status == "failed"] <- NA
    }

    # report not-overwritten files
    if (any(metadata$download_status == "already there (not downloaded)")) {
      if (
        sum(metadata$download_status == "already there (not downloaded)") ==
          nrow(metadata)
      ) {
        report_message <-
          .message(
            "All files were already there (overwritten = FALSE)",
            as = "success",
            suffix = "\n"
          )
      } else {
        unified_message <- paste(
          "{sum(metadata$download_status == 'already there (not downloaded)')}",
          "file{?s} w{?as/ere} already there (overwritten = FALSE)"
        )

        report_message <- c(
          report_message,
          "!" = paste0(cli::pluralize(unified_message))
        )
      }
    }

    # report overwritten files
    if (any(metadata$download_status == "overwritten")) {
      if (sum(metadata$download_status == "overwritten") == nrow(metadata)) {
        report_message <-
          .message(
            "All files were downloaded successfully (and all were overwritten)",
            as = "success",
            suffix = "\n"
          )
      } else {
        unified_message <- paste(
          "{sum(metadata$download_status == 'overwritten')}",
          "file{?s} w{?as/ere} downloaded (and overwritten)"
        )
        report_message <- c(
          report_message,
          "v" = paste0(cli::pluralize(unified_message))
        )
      }
    }

    # report successful files
    if (any(metadata$download_status == "saved")) {
      if (sum(metadata$download_status == "saved") == nrow(metadata)) {
        report_message <-
          .message(
            "All files were downloaded successfully",
            as = "success",
            suffix = "\n"
          )
      } else {
        unified_message <- paste(
          "{sum(metadata$download_status == 'saved')}",
          "file{?s} w{?as/ere} downloaded successfully"
        )
        report_message <- c(
          report_message,
          "v" = paste0(cli::pluralize(unified_message))
        )
      }
    }

    if (length(unique(metadata$download_status)) > 1) {
      unified_message <- paste(
        "check  the `download_status` column in the",
        "output data frame (invisibly returned) for details "
      )
      report_message <- c(report_message, "i" = unified_message)
    }

    # report download results
    if (verbose) {
      cli::cli_bullets(report_message)
    }

    # remove extra column
    metadata$non_dup_key <- NULL

    ## compute file size in MB
    metadata$file_size <- NA_real_

    # full file paths
    file_paths <- file.path(path, metadata$downloaded_file_name)

    # only for successfully available files
    existing_files <- file.exists(file_paths)

    # add size
    metadata$file_size[existing_files] <- round(
      file.info(file_paths[existing_files])$size / (1024^2),
      2
    )

    # return data frame without printing them
    invisible(metadata)
  }
