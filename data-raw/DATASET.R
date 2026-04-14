## code to prepare `DATASET` dataset goes here
# see https://r-pkgs.org/data.html#sec-data-sysdata

### Taxon code for macaulay ####
# downloaded from https://www.birds.cornell.edu/clementschecklist/introduction
#/updateindex/october-2025/2025-citation-checklist-downloads/
ml_taxon_code <- utils::read.csv(
  "./examples/eBird_taxonomy_v2025.csv",
  na.strings = ""
)

### CREATING METADATA TESTING ####
### example merged metadata ####

options(
  suwo_species = "Turdus rufiventris",
  suwo_format = "sound",
  suwo_all_data = FALSE
)

xc_adf <- query_xenocanto()
gb_adf_s <- query_gbif()
ml_adf_s <- query_macaulay(path = tempdir())

merged_metadata <- merge_metadata(xc_adf, gb_adf_s, ml_adf_s)


### CREATE ADDITIONAL METADATA FOR TESTING ####
options(suwo_all_data = TRUE)

h_sarapiquensis <- head(
  query_inaturalist(species = "Heliconia sarapiquensis", format = "image"),
  4
)
h_harpyja <- head(query_wikiaves(species = "Harpia harpyja"), 4)
a_hahneli <- query_xenocanto(
  species = 'sp:"Ameerega hahneli" cnt:"French Guiana" q:"A"',
  api_key = Sys.getenv("xc_api_key")
)
p_lotor <- head(query_gbif(species = "Procyon lotor", format = "video"), 4)
t_rufiventris <- head(
  query_macaulay(
    species = "Turdus rufiventris",
    format = "sound",
    path = tempdir()
  ),
  4
)
t_tricolor <- head(
  query_xenocanto(
    species = "Thyroptera tricolor",
    api_key = Sys.getenv("xc_api_key")
  ),
  4
)

p_striigularis <- head(
  query_macaulay(
    species = "Phaethornis striigularis",
    format = "video",
    path = tempdir()
  ),
  4
)

d_holocanthus <- query_gbif(species = "Diodon holocanthus", format = "image")

# keep only JPEG records (for simplicity for this vignette)
d_holocanthus <- d_holocanthus[d_holocanthus$file_extension == "jpeg", ]

# select 6 random JPEG records
set.seed(666)
d_holocanthus <- d_holocanthus[sample(seq_len(nrow(d_holocanthus)), 6), ]

testing_metadata <- list(
  h_harpyja = h_harpyja,
  a_hahneli = a_hahneli,
  p_lotor = p_lotor,
  t_tricolor = t_tricolor,
  t_rufiventris = t_rufiventris,
  h_sarapiquensis = h_sarapiquensis,
  p_striigularis = p_striigularis,
  d_holocanthus = d_holocanthus
)

usethis::use_data(
  ml_taxon_code,
  merged_metadata,
  testing_metadata,
  internal = TRUE,
  overwrite = TRUE,
  compress = "gzip"
)


### create function description table for vignette
metadata_list <- testing_metadata

repository <- vapply(
  metadata_list,
  function(x) {
    x[1, "repository"]
  },
  character(1)
)

repository[repository == "Wikiaves"] <- "WikiAves"

names(metadata_list) <- tolower(repository)

function_vector <- c(
  gbif = "query_gbif",
  inaturalist = "query_inaturalist",
  `macaulay library` = "query_macaulay",
  observation = "query_observation",
  wikiaves = "query_wikiaves",
  `xeno-canto` = "query_xenocanto"
)

function_vector <- function_vector[match(
  names(metadata_list),
  names(function_vector)
)]


file_types <- vapply(
  function_vector,
  function(x) {
    paste(formals(get(x))$format, collapse = ", ")
  },
  FUN.VALUE = character(1)
)
file_types <- gsub("c, ", "", file_types)
file_types[function_vector == "query_xenocanto"] <- "sound"

file_types <- gsub("getOption,|suwo_format, c", "", file_types)

# remove c(, ) and " from file_types
file_types <- gsub("\\(|\\)|\"", "", file_types)

urls <- c(
  gbif = "https://www.gbif.org/",
  inaturalist = "https://www.inaturalist.org/",
  `macaulay library` = "https://www.macaulaylibrary.org/",
  observation = "https://observation.org/",
  wikiaves = "https://www.wikiaves.com.br/",
  `xeno-canto` = "https://www.xeno-canto.org/"
)

urls <- urls[match(names(metadata_list), names(urls))]

token <- c(
  gbif = "No",
  inaturalist = "No",
  `macaulay library` = "No",
  observation = "Yes",
  wikiaves = "No",
  `xeno-canto` = "Yes"
)

token <- token[match(names(metadata_list), names(token))]


tax_level <- c(
  gbif = "Species",
  inaturalist = "Species",
  `macaulay library` = "Species",
  observation = "Species",
  wikiaves = "Species",
  `xeno-canto` = "Species, subspecies, genus, family, group"
)

tax_level <- tax_level[match(names(metadata_list), names(tax_level))]

geo_cover <- c(
  gbif = "Global",
  inaturalist = "Global",
  `macaulay library` = "Global",
  observation = "Global",
  wikiaves = "Brazil",
  `xeno-canto` = "Global"
)

geo_cover <- geo_cover[match(names(metadata_list), names(geo_cover))]

tax_coverage <- c(
  gbif = "All life",
  inaturalist = "All life",
  `macaulay library` = "Mostly birds but also other vertebrates and
  invertebrates",
  observation = NA,
  wikiaves = "Birds",
  `xeno-canto` = "Birds, frogs, non-marine mammals and grasshoppers"
)

tax_coverage <- tax_coverage[match(names(metadata_list), names(tax_coverage))]

other <- c(
  gbif = "Specify query by data base",
  inaturalist = "",
  `macaulay library` = "Interactive",
  observation = "",
  wikiaves = "",
  `xeno-canto` = "Specify query by taxonomy, geographic range and dates"
)

other <- other[match(names(metadata_list), names(other))]


colnames <- lapply(metadata_list, names)

colnames <- lapply(colnames, function(x) {
  setdiff(x, suwo:::.format_query_output(only_basic_columns = TRUE))
})

additional_data <- vapply(
  colnames,
  function(x) {
    paste(x, collapse = ", ")
  },
  FUN.VALUE = character(1)
)

additional_data <- additional_data[match(
  names(metadata_list),
  names(additional_data)
)]

query_summary <- data.frame(
  Function = function_vector,
  Repository = repository,
  `URL link` = urls,
  `File types` = file_types,
  `Requires api key` = token,
  `Taxonomic level` = tax_level,
  `Geographic coverage` = geo_cover,
  `Taxonomic coverage` = tax_coverage,
  `Additional data` = additional_data,
  `Other features` = other,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# remove duplicates
query_summary <- query_summary[!duplicated(query_summary$Repository), ]

query_summary <- query_summary[order(query_summary$Repository), ]

# Save it as a txt file
write.table(
  query_summary,
  "./vignettes/query_function_summary.txt",
  row.names = FALSE,
  sep = "\t"
)
