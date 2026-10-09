suwo: access nature media repositories
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- Project maturity -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- [![Dependencies](https://tinyverse.netlify.com/badge/suwo)](https://cran.r-project.org/package=suwo)  -->
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-Depends:-6666ff.svg)](https://cran.r-project.org/)  -->

<!-- Checks and quality -->

[![R-CMD-check](https://github.com/ropensci/suwo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/suwo/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/ropensci/suwo/workflows/pkgcheck/badge.svg)](https://github.com/ropensci/suwo/actions?query=workflow%3Apkgcheck)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/suwo/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/suwo?branch=main)
[![peer-review](https://badges.ropensci.org/729_status.svg)](https://github.com/ropensci/software-review/issues/729)

<!-- Release info -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/suwo)](https://cran.r-project.org/package=suwo)
[![packageversion](https://img.shields.io/badge/Package%20version-0.2.2-orange.svg?style=flat-square)](commits/develop)
[![Last-changedate](https://img.shields.io/badge/last%20change-2026--10--09-yellowgreen.svg)](https://github.com/ropensci/suwo/commits/main)

<!-- Usage -->

[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/suwo)](https://cran.r-project.org/package=suwo)
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/suwo)](https://cran.r-project.org/package=suwo)
<!-- badges: end -->

The [suwo](https://docs.ropensci.org/suwo/) package aims to simplify the
retrieval of nature media (mostly photos, audio files and videos) across
multiple online biodiversity databases. The five major media
repositories accessed by this package (GBIF, iNaturalist, Macaulay
Library, WikiAves, and Xeno-Canto) collectively host more than 250
million media files. Such media are increasingly used in diverse fields,
ranging from ecology and evolutionary biology (e.g., trait evolution) to
wildlife monitoring and conservation (e.g., for training species
detection models). The ability to access and download large amounts of
media files and their associated metadata from a single interface thus
provides a uniquely powerful resource for facilitating research and
conservation efforts.

The main features of the package are:

  - Obtaining media metadata from online repositories
  - Downloading associated media files
  - Updating data sets with new records

## Installing suwo

<!-- Install the package from CRAN (: -->

<!-- ```{r, eval = FALSE} -->

<!-- # install from CRAN -->

<!-- install.packages("suwo") -->

<!-- # load package -->

<!-- library(suwo) -->

<!-- ``` -->

To install the latest developmental version from
[github](https://github.com/) run:

``` r
install.packages("suwo", repos = c(
  'https://ropensci.r-universe.dev',
  'https://cloud.r-project.org'
))

#load package
library(suwo)
```

# Basic workflow for obtaining nature media files

Obtaining nature media using [suwo](https://docs.ropensci.org/suwo/)
follows a basic sequence. The following diagram illustrates this
workflow and the main functions involved:

<center>

<img src="./vignettes/workflow_diagram.png" alt="Flowchart of the suwo workflow for obtaining nature media files. Step 1, 'Get metadata', includes multiple boxes representing queries to different repositories, such as query_wikiaves() and query_xenocanto(), plus additional possible query_() calls. Arrows from all these queries converge into Step 2, 'Combine metadata', using merge_metadata() and 'Remove duplicates', using find_duplicates() and remove_duplicates(). The last step is 'Download media', using download_media(). Finally, user can update previous queries using update_metadata()" width="100%">

</center>

Take a look at the [package
vignette](https://docs.ropensci.org/suwo/articles/suwo.html) for an
overview of the workflow and the core querying functions.

## Quick example

Here is a minimal example that queries GBIF for images of a species and
downloads the resulting media files:

``` r
library(suwo)

# query GBIF for Amanita zambiana images
a_zam <- query_gbif(species = "Amanita zambiana", format = "image")

# create folder for the downloaded images
out_folder <- file.path(tempdir(), "amanita_zambiana")
dir.create(out_folder)

# download the media files
azam_files <- download_media(metadata = a_zam, path = out_folder)
```

## Articles

In addition to the package overview, these articles cover specific use
cases in more detail:

  - [Package
    overview](https://docs.ropensci.org/suwo/articles/suwo.html):
    introduces the basic workflow and core querying functions
  - [Explore geographic
    variation](https://docs.ropensci.org/suwo/articles/explore_geographic_variation.html):
    maps the geographic origin of media obtained with `suwo`
  - [Xeno-Canto
    annotations](https://docs.ropensci.org/suwo/articles/xenocanto_annotations.html):
    shows how to retrieve and work with annotations from Xeno-Canto
    recordings

## Intended use and responsible practices

The [suwo](https://docs.ropensci.org/suwo/) package is designed
exclusively for non-commercial, scientific purposes, including research,
education, and conservation. **Commercial use of data or media retrieved
through this package is the user’s responsibility and is allowed only
when the applicable license of the source database explicitly permits
such use, or when explicit, separate permission has been obtained
directly from the original source platforms or rights holders**. Users
must comply with the specific terms of service and data-use policies of
each source database, which may require attribution and may further
restrict commercial application. The package developers assume no
liability for misuse of the retrieved data or for violations of
third-party terms of service.

## Citation

Please cite [suwo](https://docs.ropensci.org/suwo/) as follows:

Araya-Salas M, Elizondo-Calvo J, Rico-Guevara A (2026). *suwo: Access
Nature Media Repositories*. R package version 0.2.2,
<https://docs.ropensci.org/suwo/>.
