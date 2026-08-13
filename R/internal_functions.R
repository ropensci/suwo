# internal helper suwo functions

# internal suwo function, not to be called by users
# allows to define internally if progress bar would be used

.pbapply_sw <- function(X, FUN, cl = 1, pbar = TRUE, Y = X, title = "", ...) {
  FUN <- match.fun(FUN)

  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    cl <- 1L
  }
  # no parallel
  if (cl == 1) {
    if (!pbar) {
      # No progress bar
      out <- lapply(seq_along(X), FUN, ...)
      return(out)
    } else {
      # progress bar
      out <- lapply(
        X = cli::cli_progress_along(
          name = title,
          x = X,
          clear = FALSE
        ),
        FUN = FUN,
        ...
      )
      return(out)
    }
    message("") # Windows rendering reset
  }

  # parallel
  if (cl > 1) {
    ## define OS
    is_windows <- (.Platform$OS.type == "windows")

    if (is_windows) {
      clust <- parallel::makeCluster(cl)
      on.exit(parallel::stopCluster(clust), add = TRUE)

      parallel::clusterExport(
        clust,
        varlist = c("FUN", "Y"),
        envir = environment()
      )
    }

    if (!pbar) {
      # No progress bar

      if (is_windows) {
        out <- parallel::parLapply(
          cl = clust,
          X = seq_along(X),
          fun = function(i) FUN(X[i], Y, ...)
        )
      } else {
        out <- parallel::mclapply(
          X = seq_along(X),
          FUN = FUN,
          mc.cores = as.integer(cl)
        )
      }

      return(out)
    } else {
      # progress bar

      nchunks <- ceiling(length(X) / as.integer(cl))

      if (cl >= length(X)) {
        chunks <- list(X)
        cl <- length(X)
      } else {
        chunks <- split(
          seq_along(X),
          cut(seq_along(X), nchunks, labels = FALSE)
        )
      }

      out <- vector("list", length(chunks))

      for (i in cli::cli_progress_along(
        name = title,
        x = seq_along(chunks),
        clear = FALSE
      )) {
        idx <- chunks[[i]]

        if (is_windows) {
          out[idx] <- parallel::parLapply(
            clust,
            idx,
            function(j) FUN(X[j], Y, ...)
          )
        } else {
          out[idx] <- parallel::mclapply(
            X = idx,
            FUN = FUN,
            mc.cores = as.integer(cl)
          )
        }
      }
      message("") # important for Windows rendering reset
      return(out)
    }
  }
}

# add emojis to messages. based on praise_emoji from testthat
.add_emoji <- function(mood) {
  if (!cli::is_utf8_output()) {
    return("")
  }

  happy_emoji <- c(
    "\U0001f600",
    # smile
    "\U0001f973",
    # party face
    "\U0001f638",
    # cat grin
    "\U0001f308",
    # rainbow
    "\U0001f947",
    # gold medal
    "\U0001f389",
    # party popper
    "\U0001f38a" # confetti ball
  )

  sad_emoji <- c(
    "\U0001f62C",
    # grimacing face
    "\U0001f635",
    # face with spiral eyes
    "\U0001f62D",
    # loudly crying face
    "\U0001f613",
    #  	weary face
    "\U0001f480",
    # skull
    "\U0001F4A9",
    # pile of poop
    "\U0001f624",
    # face with steam from nose
    "\U0001f648" # see-no-evil monkey
  )

  if (mood == "happy") {
    sample(happy_emoji, 1)
  } else {
    sample(sad_emoji, 1)
  }
}

.message <- function(
  text = "Obtaining metadata ({n} matching record{?s} found)",
  as = c("success", "warning", "failure", "error", "message"),
  n = NULL,
  suffix = "\n",
  nfiles = NULL
) {
  if (!is.null(n)) {
    text <- cli::pluralize(text)
  }

  if (as == "success") {
    if (!cli::is_utf8_output()) {
      return(text)
    }

    cli::cli_alert_success(paste0(text, " ", .add_emoji("happy"), suffix))
  }

  if (as == "warning") {
    cli::cli_alert_warning(paste0(text, " "))
  }

  if (as == "failure") {
    cli::cli_alert_danger(paste0(text, " ", .add_emoji("sad")))
  }

  if (as == "error") {
    cli::cli_alert_danger(paste0(text, " "))
  }

  if (as == "message") {
    cli::cli_alert_info(paste0(text, " "))
  }
}


# sanitize the name of a download folder
.sanitize_folder_name <- function(name, folder_by) {
  if (is.na(name)) {
    name <- paste0("unknown_", folder_by)
  }

  # Remove or replace illegal characters
  sanitized <- gsub('[<>:"/\\\\|?*]"', "_", name)

  # Remove trailing spaces and periods
  sanitized <- gsub("[ .]+$", "", sanitized)

  # Replace multiple spaces with single space
  sanitized <- gsub(" +", " ", sanitized)

  # Trim whitespace
  sanitized <- trimws(sanitized)

  # Ensure not empty after sanitization
  if (nchar(sanitized) == 0) {
    sanitized <- "unknown_folder"
  }
  return(sanitized)
}

# build_download_url
.build_download_url <- function(repo, key, file_url, file_extension = NULL) {
  switch(
    repo,

    "Xeno-Canto" = {
      key_clean <- gsub("XC", "", key)
      paste0("https://xeno-canto.org/", key_clean, "/download")
    },

    "Macaulay Library" = {
      paste0(
        "https://cdn.download.ams.birds.cornell.edu/api/v1/asset/",
        key
      )
    },

    "iNaturalist" = {
      sub("(/square|/small|/medium|/large)", "/original", file_url)
    },

    "WikiAves" = file_url,
    "GBIF" = file_url,
    "Observation" = file_url,

    file_url
  )
}

# Function to download file according to repository
.download_basic <- function(metadata, x, path, overwrite, folder_by = NULL) {
  if (!is.null(folder_by)) {
    folder_name <- .sanitize_folder_name(metadata[x, folder_by], folder_by)
    path <- normalizePath(file.path(path, folder_name))
  }

  # create path if needed
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # destination file
  destfile <- file.path(normalizePath(path), metadata$downloaded_file_name[x])

  exists <- file.exists(destfile)
  if (exists && !overwrite) {
    return("already there (not downloaded)")
  }

  # build URL
  url <- .build_download_url(
    repo = metadata$repository[x],
    key = metadata$key[x],
    file_url = metadata$file_url[x],
    file_extension = metadata$file_extension[x]
  )

  # first attempt
  dl_result <- try(
    utils::download.file(
      url = url,
      destfile = destfile,
      quiet = TRUE,
      mode = "wb",
      method = "auto",
      cacheOK = TRUE,
      extra = getOption("download.file.extra")
    ),
    silent = TRUE
  )

  # retry if failed
  if (.is_error(dl_result)) {
    Sys.sleep(0.5)
    dl_result <- try(
      utils::download.file(
        url = url, # reuse same URL
        destfile = destfile,
        quiet = TRUE,
        mode = "wb",
        method = "auto",
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
      ),
      silent = TRUE
    )
  }

  # result handling
  if (.is_error(dl_result)) {
    return("failed")
  } else {
    if (exists) {
      return("overwritten")
    } else {
      return("saved")
    }
  }
}

# suppressing warnings from download.file
.download <- function(...) {
  suppressWarnings(.download_basic(...))
}

# fix extension so it is homogeneous across functions
.fix_extension <- function(x, url) {
  # if the strings contains "?" extract string before "?"
  if (any(grepl("\\?", x))) {
    x <- gsub("\\?.*$", "", x)
  }

  # if is NA then try to get from url
  if (any(is.na(x))) {
    url_ext <- tools::file_ext(url[is.na(x)])
    url_ext[url_ext %in% c("", ".", " ")] <- NA
    x[is.na(x)] <- url_ext
  }

  # jpg to jpeg
  x <- ifelse(grepl("jpg$|jpeg$", x, ignore.case = TRUE), "jpeg", x)

  # png
  x <- ifelse(grepl("png$", x, ignore.case = TRUE), "png", x)

  # gif
  x <- ifelse(grepl("gif$", x, ignore.case = TRUE), "gif", x)

  # tif to tiff
  x <- ifelse(grepl("tif$", x, ignore.case = TRUE), "tiff", x)

  # mpeg to mp3
  x <- ifelse(grepl("mpeg$|mpga$|mpeg3$", x, ignore.case = TRUE), "mp3", x)

  # x-wav to wav
  x <- ifelse(grepl("x-wav$|vnd.wave$", x, ignore.case = TRUE), "wav", x)

  # x-m4a to m4a
  x <- ifelse(grepl("x-m4a$|mp4$", x, ignore.case = TRUE), "m4a", x)

  # convert to lower case
  x <- tolower(x)

  # for Macaulay
  x <- ifelse(x == "audio", "mp3", x)
  x <- ifelse(x == "photo", "jpeg", x)
  x <- ifelse(x == "video", "mp4", x)

  # return
  return(x)
}

# rbind all data frames in a list after making them have the same columns
# X must be a list of data frames
.merge_data_frames <- function(X) {
  # get common names to all data frames in X
  common_names <- unique(unlist(lapply(X, names)))

  # add missing columns to all data frames in X
  Y <- lapply(X, function(e) {
    nms <- names(e)
    if (length(nms) != length(common_names)) {
      for (o in common_names[!common_names %in% nms]) {
        e <-
          data.frame(e, NA, stringsAsFactors = FALSE, check.names = FALSE)
        names(e)[ncol(e)] <- o
      }
    }
    return(e)
  })

  # all results in a single data frame
  Z <- do.call(rbind, Y)

  Z
}

# Custom function for names with particles
.to_name_case <- function(x) {
  result <- character(length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- NA
    } else {
      result[i] <- tools::toTitleCase(tolower(x[i]))
    }
  }
  return(result)
}

# format query output dataframe to standardize column names
.format_query_output <- function(
  X,
  column_names,
  all_data,
  format,
  raw_data = FALSE,
  call,
  input_file = NA,
  only_basic_columns = FALSE
) {
  if (raw_data) {
    return(X)
  }

  basic_colums <- c(
    "repository",
    "format",
    "key",
    "species",
    "date",
    "time",
    "user_name",
    "country",
    "locality",
    "latitude",
    "longitude",
    "file_url",
    "file_extension",
    "observation_url"
  )
  if (only_basic_columns) {
    return(basic_colums)
  }

  # if missing any basic column add it with NAs
  for (i in basic_colums) {
    if (is.null(X[[i]])) {
      X[[i]] <- NA
    }
  }

  # lower case
  names(X) <- tolower(names(X))
  names(column_names) <- tolower(names(column_names))

  names_df <- data.frame(old = names(column_names), new = column_names)

  for (i in seq_len(nrow(names_df))) {
    # if old name exists
    if (any(names(X) == names_df$old[i])) {
      names(X)[names(X) == names_df$old[i]] <- names_df$new[i]
    } else {
      # if expected (old) name does not exist add new column with NAs
      X <- data.frame(X, NA, stringsAsFactors = FALSE, check.names = FALSE)
      names(X)[ncol(X)] <- names_df$new[i]
    }
  }

  # replace "." with "_"
  names(X) <- gsub("\\.|\\-", "_", names(X))

  # replace "__" with "_"
  names(X) <- gsub("___", "_", names(X), fixed = TRUE)

  if (is.null(X$latitude)) {
    X$latitude <- NA
  }

  if (is.null(X$longitude)) {
    X$longitude <- NA
  }

  # convert lat long to numbers
  X$latitude <- as.numeric(X$latitude)
  X$longitude <- as.numeric(X$longitude)

  # add format
  X$format <- format

  # repo
  X$repository <- .repo_from_call(call)

  # format time column
  if (X$repository[1] == "Xeno-Canto") {
    X$time[X$time == "?"] <- NA
  }

  if (X$repository[1] == "WikiAves") {
    X$time <- NA
  }

  if (X$repository[1] == "GBIF") {
    X$time <- substr(x = X$time, start = 1, stop = 5)
  }

  if (X$repository[1] == "iNaturalist") {
    X$time <- substr(x = X$time, start = 12, stop = 16)

    # fix image size in URL
    X$file_url <- gsub("square", "original", X$file_url)
  }

  # fix time
  ## for Macaulay first
  if (X$repository[1] == "Macaulay Library") {
    X$time <- ifelse(nchar(X$time) == 3, paste0(0, X$time), X$time)

    X$time <- paste0(substr(X$time, 1, 2), ":", substr(X$time, 3, 4))
  }

  # homogenize time
  X$time <- .convert_times(X$time)

  # homogenize date
  X$date <- .homogenize_dates(X$date)

  # fix extension
  X$file_extension <- .fix_extension(X$file_extension, url = X$file_url)

  # replace "" with NA
  X$country[X$country == ""] <- NA
  X$locality[X$locality == ""] <- NA

  # user name case
  X$user_name <- .to_name_case(X$user_name)

  # order so basic columns go first
  non_basic_colms <- setdiff(names(X), basic_colums)
  X <- X[, c(basic_colums, non_basic_colms)]

  if (!all_data) {
    # remove columns that are not basic
    X <- X[, basic_colums]
  }

  # remove rows with NAs in URL
  if (anyNA(X$file_url)) {
    # get observations without URL
    excluded_results <- X[is.na(X$file_url), ]

    # save as an attribute
    attributes(X)$excluded_results <- excluded_results

    # let users know some observations were excluded
    .message(
      text = paste0(
        "{n} observation{?s} d{?oes/o} not have a download link and w{?as/ere}",
        " removed from the results (included as an attribute called",
        " 'excluded_results')",
        ".\n"
      ),
      as = "warning",
      n = sum(is.na(X$file_url))
    )

    # remove those observations
    X <- X[!is.na(X$file_url), ]
  }

  # drop additional levels
  X <- droplevels(X)

  return(X)
}

# get repo name from call
.repo_from_call <- function(x) {
  # get function symbol as string
  x <- as.character(x[[1]])

  # drop namespace if any (suwo::fun)
  x <- grep(pattern = "query", x = x, value = TRUE)

  switch(
    strsplit(
      x = as.character(x),
      split = "(",
      fixed = TRUE
    )[[1]][1],
    query_wikiaves = "WikiAves",
    query_xenocanto = "Xeno-Canto",
    query_gbif = "GBIF",
    query_inaturalist = "iNaturalist",
    query_macaulay = "Macaulay Library"
  )
}

## function to split macaulay queries by year-month
.date_ranges <- function(x) {
  x <- sort(x)
  # current year as year.decimal
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_month <- as.numeric(format(Sys.Date(), "%m"))

  unique_years <- floor(min(x)):floor(max(x))

  # determine if has to be split by month
  if (!all(x == floor(x))) {
    # Calculate fraction of the year (0 = Jan, 1/12 ≈ 0.0833 per month)
    current_date <- current_year + (current_month) / 12

    # filter dates in the future (higher than current date)
    x <- x[x <= current_date]

    if (current_year %in% unique_years) {
      x <- c(x, current_date)
    }

    first_year <- min(floor(x))
    last_year <- max(floor(x)) + 1 # add one to include the end of the year

    # create df with all possible month year combinations
    poss_month_year_df <- expand.grid(month = 1:12, year = first_year:last_year)

    poss_month_year_df$year_decimal <-
      poss_month_year_df$year + (poss_month_year_df$month - 1) / 12

    date_list <- lapply(seq_along(x[-1]), function(y) {
      time_diff <- poss_month_year_df$year_decimal - x[y]

      start <- poss_month_year_df[
        poss_month_year_df$year_decimal ==
          poss_month_year_df$year_decimal[
            time_diff >= 0
          ][1],
      ]
      time_diff <- poss_month_year_df$year_decimal - x[y + 1]
      end <- poss_month_year_df[
        poss_month_year_df$year_decimal ==
          rev(poss_month_year_df$year_decimal[
            time_diff < 0
          ])[1],
      ]

      out <- cbind(start[, 1:2], end[, 1:2])

      names(out) <- c("start_month", "start_year", "end_month", "end_year")
      return(out)
    })

    dates_df <- do.call(rbind, date_list)

    # expand rows that cross years
    dates_list <- lapply(seq_len(nrow(dates_df)), function(i) {
      Y <- dates_df[i, ]
      if (Y$start_month > Y$end_month | Y$start_year < Y$end_year) {
        Y <- data.frame(
          start_month = c(Y$start_month, 1),
          start_year = c(Y$start_year, Y$start_year + 1),
          end_month = c(12, Y$end_month),
          end_year = c(Y$start_year, Y$start_year + 1)
        )
      }
      Y
    })

    dates_df <- do.call(rbind, dates_list)
  } else {
    # remove years above current year
    unique_years <- unique_years[unique_years <= current_year]

    dates_df <- data.frame(
      start_month = 1,
      start_year = x[-length(x)],
      end_month = 12,
      end_year = c(x[-c(1, length(x))] - 1, x[length(x)]),
      stringsAsFactors = FALSE
    )
  }
  return(dates_df)
}

# monitor if a new file is added (with .csv extension) to a folder
# and return the name of the new file
.monitor_new_files <- function(path, interval = 1) {
  stopifnot(dir.exists(path))

  old_files <- list.files(path, pattern = "\\.csv$", full.names = FALSE)

  repeat {
    Sys.sleep(interval)

    new_files <- list.files(path, pattern = "\\.csv$", full.names = FALSE)
    added <- setdiff(new_files, old_files)

    if (length(added) > 0) {
      file_path <- file.path(path, added[1])

      # wait until file exists and stops growing
      last_size <- -1

      repeat {
        info <- file.info(file_path)

        # file.info() may briefly return NA while file is being created
        if (!is.na(info$size) && info$size == last_size) {
          break
        }

        last_size <- info$size
        Sys.sleep(0.5)
      }

      return(added[1])
    }
  }
}

# clean not valid date format (must be "YYYY-MM-DD"),
# if possible extracts year from various formats, using current year
# as upper bound
.clean_dates <- function(date_strings) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  vapply(
    date_strings,
    function(date_str) {
      if (is.na(date_str)) {
        return(NA_character_)
      }

      # First check if it's valid YYYY-MM-DD format
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
        # Validate month and day ranges
        parts <- strsplit(date_str, "-")[[1]]
        year <- as.numeric(parts[1])
        month <- as.numeric(parts[2])
        day <- as.numeric(parts[3])

        if (
          month >= 1 &
            month <= 12 &
            day >= 1 &
            day <= 31 &
            year <= current_year
        ) {
          return(date_str) # Keep the full valid date
        }
      }

      # If not valid YYYY-MM-DD, try to extract just the year
      year_match <- regmatches(date_str, regexpr("\\d{4}", date_str))
      if (length(year_match) > 0) {
        year <- as.numeric(year_match[1])
        #Return just the year if it's reasonable (between 1900 and current year)
        if (year >= 1900 & year <= current_year) {
          return(as.character(year))
        }
      }

      # If we can't extract a reasonable year, return NA
      return(NA_character_)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

# homogenize dates
.homogenize_dates <- function(date_strings) {
  date_strings <- vapply(
    date_strings,
    function(date_str) {
      # If input is NA, return NA
      if (is.na(date_str)) {
        return(NA_character_)
      }

      # Try to parse the date
      parsed <- lubridate::parse_date_time(
        date_str,
        orders = c("dmy", "ymd", "ymd HMS", "ymd HM", "ymd"),
        truncated = 2,
        quiet = TRUE
      )

      # If parsing succeeded, convert to Date format
      if (!is.na(parsed)) {
        as.character(as.Date(parsed))
      } else {
        # If parsing failed but input is not NA, return the input value
        date_str
      }
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # remove invalid dates
  date_strings <- .clean_dates(date_strings)
}

# Function to check if a string is a valid time
.is_valid_time <- function(time_str) {
  if (is.na(time_str)) {
    return(FALSE)
  }

  # Remove spaces and convert to lowercase for easier matching
  clean_str <- tolower(trimws(time_str))

  # Check for invalid patterns first
  if (
    grepl("^\\?|xx|morning|^[a-z]+$", clean_str) &&
      !grepl("am|pm", clean_str)
  ) {
    return(FALSE)
  }

  # Check for various valid time patterns
  # HH:MM, H:MM, HH.MM, H.MM patterns with optional AM/PM
  if (
    grepl("^\\d{1,2}[:.]\\d{2}\\s*(am|pm)?$", clean_str) ||
      grepl("^\\d{1,2}\\s*(am|pm)$", clean_str)
  ) {
    return(TRUE)
  }

  return(FALSE)
}

# Function to convert time strings to standardized format
.convert_times <- function(time_strings) {
  vapply(
    time_strings,
    function(time_str) {
      if (is.na(time_str) || !.is_valid_time(time_str)) {
        return(NA_character_)
      }

      # Clean the string
      clean_str <- tolower(trimws(time_str))

      # Handle simple am/pm without time (like "6am")
      if (grepl("^\\d{1,2}\\s*(am|pm)$", clean_str)) {
        time_num <- as.numeric(sub("\\s*(am|pm)$", "", clean_str))
        period <- ifelse(grepl("pm$", clean_str), "pm", "am")

        # Convert to 24-hour format
        if (period == "pm" && time_num < 12) {
          hours <- time_num + 12
        } else if (period == "am" && time_num == 12) {
          hours <- 0
        } else {
          hours <- time_num
        }

        return(sprintf("%02d:00", hours))
      }

      # Handle times with AM/PM
      if (grepl("am|pm", clean_str)) {
        time_part <- sub("\\s*(am|pm)$", "", clean_str)
        period <- ifelse(grepl("pm$", clean_str), "pm", "am")

        # Handle both colon and dot separators
        if (grepl("[:.]", time_part)) {
          parts <- strsplit(time_part, "[:.]")[[1]]
        } else {
          # If no separator, assume it's just hours
          parts <- c(time_part, "00")
        }

        hours <- as.numeric(parts[1])
        minutes <- ifelse(length(parts) > 1, as.numeric(parts[2]), 0)

        # Convert to 24-hour format
        if (period == "pm" && hours < 12) {
          hours <- hours + 12
        } else if (period == "am" && hours == 12) {
          hours <- 0
        }

        return(sprintf("%02d:%02d", hours, minutes))
      }

      # Handle 24-hour times without AM/PM
      if (grepl("[:.]", clean_str)) {
        parts <- strsplit(clean_str, "[:.]")[[1]]
      } else {
        # If no separator, assume it's just hours
        parts <- c(clean_str, "00")
      }

      hours <- as.numeric(parts[1])
      minutes <- ifelse(length(parts) > 1, as.numeric(parts[2]), 0)

      # Standardize to HH:MM format
      sprintf("%02d:%02d", hours, minutes)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}


## function to check arguments
.check_arguments <- function(fun, args) {
  # make function name a character
  fun <- as.character(fun)[1]

  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()

  ### check arguments
  if (!is.null(args$species)) {
    checkmate::assert_multi_class(
      x = args$species,
      classes = c("character", "factor"),
      add = check_collection,
      .var.name = "species"
    )

    checkmate::assert_vector(
      x = args$species,
      add = check_collection,
      len = 1,
      .var.name = "species"
    )
  }

  if (!is.null(args$format)) {
    checkmate::assert_multi_class(
      x = args$format,
      classes = c("character"),
      add = check_collection,
      .var.name = "format"
    )
  }

  if (!is.null(args$cores)) {
    checkmate::assert_integerish(
      args$cores,
      add = check_collection,
      lower = 1,
      upper = parallel::detectCores(),
      .var.name = "cores"
    )
  }

  if (!is.null(args$pb)) {
    checkmate::assert_logical(
      x = args$pb,
      len = 1,
      add = check_collection,
      .var.name = "pb"
    )
  }

  if (!is.null(args$type)) {
    checkmate::assert_multi_class(
      x = args$type,
      classes = c("character"),
      add = check_collection,
      .var.name = "type"
    )
  }

  if (!is.null(args$marker_color)) {
    checkmate::assert_vector(
      x = args$marker_color,
      any.missing = FALSE,
      all.missing = FALSE,
      min.len = 1,
      null.ok = FALSE,
      add = check_collection,
      .var.name = "marker_color"
    )

    .valid_marker_colors <- c(
      "red",
      "darkred",
      "lightred",
      "orange",
      "beige",
      "green",
      "darkgreen",
      "lightgreen",
      "blue",
      "darkblue",
      "lightblue",
      "purple",
      "darkpurple",
      "pink",
      "cadetblue",
      "white",
      "gray",
      "lightgray",
      "black"
    )

    checkmate::assert_subset(
      x = args$marker_color,
      choices = .valid_marker_colors,
      add = check_collection,
      .var.name = "marker_color"
    )
  }

  if (!is.null(args$palette)) {
    checkmate::assert_function(
      x = args$palette,
      null.ok = FALSE,
      add = check_collection,
      .var.name = "palette"
    )
  }

  if (!is.null(args$tags)) {
    checkmate::assert_multi_class(
      x = args$tags,
      classes = c("character"),
      add = check_collection,
      .var.name = "tags"
    )
  }

  if (!is.null(args$popup_size)) {
    checkmate::assert_numeric(
      x = args$popup_size,
      len = 1,
      lower = 1,
      all.missing = FALSE,
      any.missing = FALSE,
      null.ok = FALSE,
      add = check_collection,
      .var.name = "popup_size"
    )
  }

  if (!is.null(args$cluster)) {
    checkmate::assert_logical(
      x = args$cluster,
      len = 1,
      add = check_collection,
      .var.name = "cluster"
    )
  }

  if (!is.null(args$overwrite)) {
    checkmate::assert_logical(
      x = args$overwrite,
      len = 1,
      add = check_collection,
      .var.name = "overwrite"
    )
  }

  if (!is.null(args$verbose)) {
    checkmate::assert_logical(
      x = args$verbose,
      len = 1,
      add = check_collection,
      .var.name = "verbose"
    )
  }

  if (!is.null(args$identified)) {
    checkmate::assert_logical(
      x = args$identified,
      len = 1,
      add = check_collection,
      .var.name = "identified"
    )
  }

  if (!is.null(args$verifiable)) {
    checkmate::assert_logical(
      x = args$verifiable,
      len = 1,
      add = check_collection,
      .var.name = "verifiable"
    )
  }

  if (!is.null(args$dataset)) {
    if (!is.null(args$dataset)) {
      checkmate::assert_multi_class(
        x = args$dataset,
        classes = c("character"),
        add = check_collection,
        .var.name = "dataset"
      )
    }
  }

  if (!is.null(args$all_data)) {
    checkmate::assert_logical(
      x = args$all_data,
      len = 1,
      add = check_collection,
      .var.name = "all_data"
    )
  }

  if (!is.null(args$path)) {
    if (!is.null(args$path)) {
      checkmate::assert_directory(
        x = args$path,
        access = "r",
        add = check_collection,
        .var.name = "path"
      )
    }
  }

  ## access_wikiaves() arguments -------------------------------------------
  # chrome_bin and user_data_dir default to NULL (triggering auto-detection
  # in access_wikiaves() itself), so a NULL value is intentionally left
  # unchecked here, consistent with how other optional arguments above are
  # handled

  if (!is.null(args$chrome_bin)) {
    checkmate::assert_string(
      x = args$chrome_bin,
      min.chars = 1,
      add = check_collection,
      .var.name = "chrome_bin"
    )
  }

  if (!is.null(args$port)) {
    checkmate::assert_integerish(
      x = args$port,
      len = 1,
      lower = 1,
      upper = 65535,
      add = check_collection,
      .var.name = "port"
    )
  }

  if (!is.null(args$url)) {
    checkmate::assert_string(
      x = args$url,
      pattern = "^https?://",
      add = check_collection,
      .var.name = "url"
    )
  }

  if (!is.null(args$timeout)) {
    checkmate::assert_number(
      x = args$timeout,
      lower = 0,
      finite = TRUE,
      add = check_collection,
      .var.name = "timeout"
    )
  }

  if (!is.null(args$launch_wait)) {
    checkmate::assert_number(
      x = args$launch_wait,
      lower = 0,
      finite = TRUE,
      add = check_collection,
      .var.name = "launch_wait"
    )
  }

  if (!is.null(args$challenge_wait)) {
    checkmate::assert_number(
      x = args$challenge_wait,
      lower = 0,
      finite = TRUE,
      add = check_collection,
      .var.name = "challenge_wait"
    )
  }

  if (!is.null(args$user_data_dir)) {
    checkmate::assert_string(
      x = args$user_data_dir,
      min.chars = 1,
      add = check_collection,
      .var.name = "user_data_dir"
    )
  }

  if (!is.null(args$set_env)) {
    checkmate::assert_logical(
      x = args$set_env,
      len = 1,
      any.missing = FALSE,
      add = check_collection,
      .var.name = "set_env"
    )
  }

  if (!is.null(args$metadata)) {
    checkmate::assert_multi_class(
      x = args$metadata,
      classes = c("data.frame"),
      add = check_collection,
      .var.name = "metadata"
    )

    must_columns <- .format_query_output(only_basic_columns = TRUE)

    if (fun == "remove_duplicates") {
      must_columns <- c(must_columns, "duplicate_group")
    }

    checkmate::assertNames(
      x = names(args$metadata),
      must.include = must_columns,
      add = check_collection,
      .var.name = "column names in metadata"
    )

    if (!is.null(args$folder_by)) {
      checkmate::assert_vector(
        x = args$folder_by,
        add = check_collection,
        any.missing = TRUE,
        null.ok = TRUE,
        len = 1,
        .var.name = "folder_by"
      )

      out <- try(
        checkmate::assert_multi_class(
          x = args$metadata[, args$folder_by, drop = TRUE],
          classes = c("character", "factor"),
          add = check_collection,
          .var.name = "folder_by"
        ),
        silent = TRUE
      )
    }

    if (fun == "download_media") {
      # check if NAs in repository column
      checkmate::assert_vector(
        x = args$metadata$repository,
        add = check_collection,
        any.missing = FALSE,
        .var.name = "'repository' column in metadata"
      )

      checkmate::assert_vector(
        x = args$metadata$species,
        add = check_collection,
        any.missing = FALSE,
        .var.name = "'species' column in metadata"
      )

      checkmate::assert_vector(
        x = args$metadata$key,
        add = check_collection,
        any.missing = FALSE,
        .var.name = "'key' column in metadata"
      )

      checkmate::assert_vector(
        x = args$metadata$file_extension,
        add = check_collection,
        any.missing = FALSE,
        .var.name = "'file_extension' column in metadata"
      )
    }
  }

  return(check_collection)
}

# custom reporting of errors
.report_assertions <- function(collection) {
  checkmate::assert_class(collection, "AssertCollection")
  if (!collection$isEmpty()) {
    msgs <- collection$getMessages()

    msgs <- gsub("^Variable", "", msgs)

    context <- "%i input arguments failed:"
    err <- c(sprintf(context, length(msgs)), strwrap(msgs, prefix = " * "))
    stop(simpleError(paste0(err, collapse = "\n"), call = sys.call(1L)))
  }
  invisible(TRUE)
}

## check internet
# gracefully fail if internet resource is not available
.checkconnection <- function(
  service = c(
    "gbif",
    "inat",
    "macaulay",
    "wikiaves",
    "xenocanto",
    "observation"
  ),
  verb = TRUE
) {
  # set user agent option globally
  options(HTTPUserAgent = "suwo (https://github.com/ropensci/suwo)")

  # check if internet is available
  if (!httr2::is_online()) {
    if (verb) {
      .message(
        "No internet connection available (check your connection!)",
        as = "failure"
      )
    }
    return(FALSE)
  }

  if (!is.null(service)) {
    service <- match.arg(service)

    urls <- list(
      gbif = "https://api.gbif.org/",
      inat = "https://www.inaturalist.org/",
      macaulay = "https://www.macaulaylibrary.org/",
      wikiaves = "https://www.wikiaves.com.br",
      xenocanto = "https://www.xeno-canto.org",
      observation = "https://observation.org"
    )

    messages <- list(
      gbif = "GBIF API",
      inat = "INaturalist",
      macaulay = "macaulaylibrary.org",
      wikiaves = "wikiaves.com.br",
      xenocanto = "xeno-canto.org",
      observation = "https://observation.org"
    )

    url <- urls[[service]]
    name <- messages[[service]]

    # Attempt request
    req <- httr2::request(url)

    req <- httr2::req_user_agent(
      req,
      "suwo (https://github.com/ropensci/suwo)"
    )

    req <- httr2::req_error(
      req,
      is_error = function(resp) FALSE
    )

    response <- try(
      httr2::req_perform(req),
      silent = TRUE
    )

    # a request that never completed at all (DNS failure, timeout, TLS
    # error, etc.) is a genuine connectivity problem regardless of service
    if (.is_error(response)) {
      if (verb) {
        .message(paste("No connection to", name), as = "failure")
      }
      return(FALSE)
    }

    # WikiAves sits behind Cloudflare's bot protection, which blocks plain
    # (non-browser) requests with an HTTP 403 even when the site itself is
    # perfectly healthy and reachable. For this service specifically, an
    # HTTP error status therefore does NOT indicate a connection problem
    # -- receiving any response at all (even a 403) already confirms
    # DNS/TLS/network all worked, so it is treated as "connected". This
    # does not apply to the other services, where an HTTP error status is
    # still treated as a real connection failure.
    is_wikiaves_bot_block <- service == "wikiaves" &&
      httr2::resp_is_error(response)

    if (!is_wikiaves_bot_block && httr2::resp_is_error(response)) {
      if (verb) {
        .message(paste("No connection to", name), as = "failure")
      }
      return(FALSE)
    }

    if (!is_wikiaves_bot_block) {
      content <- httr2::resp_body_string(response, encoding = "UTF-8")
      if (grepl("Could not connect to the database", content)) {
        if (verb) {
          .message(paste(name, "website is apparently down"), as = "failure")
        }
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# look up species taxon code for Macaulay queries
.taxon_code_search <-
  function(species = getOption("suwo_species"), ml_taxon_code = ml_taxon_code) {
    taxon_code <- ml_taxon_code$SPECIES_CODE[
      ml_taxon_code$SCI_NAME == species &
        !is.na(ml_taxon_code$SCI_NAME)
    ]
    if (length(taxon_code) > 0) {
      return(taxon_code[1])
    } else {
      return(NULL)
    }
  }

# get if an object is an error from try()
.is_error <- function(x) {
  base::inherits(x, "try-error")
}

# make audio html for popup (map_locations)
.make_media_html <- function(metadata) {
  out <- mapply(
    function(repo, key, url, format) {
      # Xeno-Canto (AUDIO)
      if (repo == "Xeno-Canto") {
        key_clean <- gsub("XC", "", key)
        audio_url <- paste0("https://xeno-canto.org/", key_clean, "/download")
        player_url <- paste0("https://xeno-canto.org/", key_clean, "/player")

        return(paste0(
          "<div style='width:100%; text-align:center;'>",

          "<audio controls preload='none' style='width:100%;'>",
          "<source src='",
          audio_url,
          "'>",
          "</audio>",

          "<div style='font-size:0.8em; margin-top:4px;'>",
          "<a href='",
          player_url,
          "' target='_blank' rel='noopener noreferrer'>",
          "View dynamic spectrogram on Xeno-Canto</a>",
          "</div>",

          "</div>"
        ))
      }

      # IMAGE
      if (format == "image") {
        return(paste0(
          "<a href='",
          url,
          "' target='_blank' style='display:block;'>",
          "<img src='",
          url,
          "' style='width:100%; max-height:250px; object-fit:contain;'>",
          "</a>"
        ))
      }

      # VIDEO
      if (format == "video") {
        # direct video file
        if (grepl("\\.mp4$|\\.webm$|\\.ogg$", url, ignore.case = TRUE)) {
          return(paste0(
            "<video controls preload='none' style='width:100%; height:auto;'>",
            "<source src='",
            url,
            "'>",
            "</video>"
          ))
        }

        # Macaulay video
        if (repo == "Macaulay Library") {
          video_url <- paste0(sub("/$", "", url), "/video")
          return(paste0(
            "<video controls preload='none' style='width:100%; height:auto;'>",
            "<source src='",
            video_url,
            "' type='video/mp4'>",
            "</video>"
          ))
        }

        # iframe fallback
        return(paste0(
          "<iframe src='",
          url,
          "' frameborder='0' allow='encrypted-media' ",
          "style='width:100%; aspect-ratio:16/9;'></iframe>"
        ))
      }

      # AUDIO (default)
      if (format == "sound") {
        # Macaulay Library
        if (repo == "Macaulay Library") {
          asset_url <- paste0("https://macaulaylibrary.org/asset/", key)

          return(paste0(
            "<div style='width:100%; text-align:center;'>",

            "<audio controls preload='none' style='width:100%;'>",
            "<source src='",
            url,
            "'>",
            "</audio>",

            "<div style='font-size:0.8em; margin-top:4px;'>",
            "<a href='",
            asset_url,
            "' target='_blank' rel='noopener noreferrer'>",
            "View dynamic spectrogram on Macaulay Library</a>",
            "</div>",

            "</div>"
          ))
        }

        # other audio
        return(paste0(
          "<audio controls preload='none' style='width:100%;'>",
          "<source src='",
          url,
          "'>",
          "</audio>"
        ))
      }

      # fallback
      return(paste0(
        "<a href='",
        url,
        "' target='_blank' rel='noopener noreferrer'>View media</a>"
      ))
    },
    metadata$repository,
    metadata$key,
    metadata$file_url,
    metadata$format,
    SIMPLIFY = FALSE
  )

  unlist(out, use.names = FALSE)
}

# check if API key is supplied and is not "" or NULL
.check_api_key <- function(api_key) {
  if (is.null(api_key) || !nzchar(api_key)) {
    cli::cli_abort(c(
      "An API key is required for Xeno-Canto API v3.",
      "i" = "Get yours at {.url https://xeno-canto.org/account}.",
      "i" = paste(
        "Once you have it, set it as an environment variable:",
        "{.code Sys.setenv(xc_api_key = 'YOUR_API_KEY_HERE')}"
      )
    ))
  }
}

# print message only once in a session
.wait_once <- function(delay) {
  if (is.null(getOption("suwo_wait_done"))) {
    Sys.sleep(delay)
    options(suwo_wait_done = TRUE)
  }
}
