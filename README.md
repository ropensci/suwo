suwo: access nature media repositories
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- [![Dependencies](https://tinyverse.netlify.com/badge/suwo)](https://cran.r-project.org/package=suwo)  -->
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-Depends:-6666ff.svg)](https://cran.r-project.org/)  -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.2-orange.svg?style=flat-square)](commits/develop)  -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2026--04--28-yellowgreen.svg)](https://github.com/ropensci/suwo/commits/main)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/suwo/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/suwo?branch=main)
[![pkgcheck](https://github.com/ropensci/suwo/workflows/pkgcheck/badge.svg)](https://github.com/ropensci/suwo/actions?query=workflow%3Apkgcheck)  
<!-- [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/suwo)](https://cran.r-project.org/package=suwo) -->
<!-- [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/suwo)](https://cranlogs.r-pkg.org/badges/grand-total/suwo) -->
[![R-CMD-check](https://github.com/ropensci/suwo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/suwo/actions/workflows/R-CMD-check.yaml)
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
