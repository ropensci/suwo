#' @param cores Numeric vector of length 1. Controls whether parallel computing
#' is applied by specifying the number of cores to be used. Default is 1
#' (i.e. no parallel computing). Can be set globally for the current R session
#' via the "mc.cores" option (e.g.  `options(mc.cores = 2)`). Note that
#' some repositories might not support parallel queries from the same IP
#' address as it might be identified as denial-of-service cyberattack.
#' @param pb Logical argument to control if progress bar is shown. Default
#' is `TRUE`. Can be set globally for the current R session via the "suwo_pb"
#' option ( `options(suwo_pb = TRUE)`). Not shown if only a few observations
#' are found.
#' @param species Character string with the scientific name of a species in
#' the format: "Genus epithet". Required. Can be set globally for the current
#' R session via the "suwo_species" option
#' (e.g. `options(suwo_species = "Hypsiboas rufitelus")`).
#' @param verbose Logical argument that determines if text is shown in
#' console. Default is `TRUE`. Can be set globally for the current R
#' session via the "suwo_verbose" option
#' ( `options(suwo_verbose = TRUE)`).
#' @param all_data Logical argument that determines if all data available
#' from database is shown in the results of search. Default is `FALSE`.
#' Can be set globally for the current R session via the "suwo_all_data"
#' option ( `options(suwo_all_data = TRUE)`).
#' @param raw_data Logical argument that determines if the raw data from the
#' repository is returned (e.g. without any manipulation).
#' Default is `FALSE`. Can be set globally for the current R session
#' via the "suwo_raw_data" option ( `options(suwo_raw_data = TRUE)`). If `TRUE`
#' `all_data` is set to `TRUE` internally. Useful for developers,
#' or if users suspect that some data is mishandled during processing
#' (i.e. date information is lost). Note that the metadata obtained
#' when `raw_data = TRUE` is not standardized, so most suwo functions
#' for downstream steps will not work on them.
#' @param metadata Data frame with the metadata of media records. Typically the
#' output of one of the query functions in this package
#' (e.g. [query_gbif()], [query_inaturalist()], etc.).
#' @name template_params
#' @keywords internal
#' @title template_params
NULL
