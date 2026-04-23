suwo 0.2.1
=========================

### Minor changes

* Added 'file_size' column to `download_media()` output to provide users with information about the size of downloaded media files and potentially help with detecting duplicates.


suwo 0.2.1
=========================

### Minor changes

* Added images, audio player and video player to the popups of `map_locations()` for better visualization of media types.

suwo 0.2.0
=========================

### Major changes

* Refactored all HTTP requests to rely more consistently on **httr2**, including:
  * Use of `req_url_query()` instead of manual URL string concatenation.
  * Use of `resp_body_json()` where applicable.
* Unified argument order across all `query_*()` functions for a more consistent user interface.
* Default media format changed from `"sound"` to `"image"` where applicable.
* Global options are now namespaced with `suwo_` to avoid conflicts (e.g. `suwo_verbose`, `suwo_format`).
* Improved pagination and request robustness for GBIF, WikiAves, Xeno-Canto, and iNaturalist queries.
* Progress reporting refactored to use **cli** instead of **pbapply**.
* Migrated roxygen documentation to **Markdown syntax**.
* Reformatted the codebase using **air**, with a GitHub Actions workflow to enforce formatting.
* Dropped unused or unnecessary dependencies (`methods`, `viridis`), updated minimum R version to ≥ 4.0.0, and moved vignette-only packages to `Suggests`.
* Replaced mc.cores option with suwo_cores to allow users to set parallelization consistently across suwo functions within an R session.

### API keys & security

* Xeno-Canto API keys are now read exclusively from environment variables via `Sys.getenv("xc_api_key")`.
* Removed encouragement of API keys being passed directly in function calls or stored in options.

### Validation & robustness

* Added stricter input validation:
  * Enforced single-species queries where required.
  * Improved error handling for incorrect inputs to `remove_duplicates()`.
* Metadata records without associated media URLs are now stored as an attribute of the returned data frame instead of global options.
* Improved handling of duplicated media records and clarified their interpretation in documentation.
* Replaced usage of `methods::is()` with `inherits()`.

### Documentation & usability

* Expanded vignette explanations, including clearer alt text for figures.
* Clarified licensing and responsibility for commercial use of retrieved data.
* Improved guidance for handling duplicated records.
* README rendering is now triggered only when `README.Rmd` changes.

### Testing & maintenance

* Cleaned up test infrastructure and fixed Codecov integration.
* Removed duplicated `.onLoad()` definitions.
* Simplified internal code by removing unnecessary `paste()` calls and replacing `seq_len(length())` with `seq_along()`.

---

suwo 0.1.0
=========================

* First release
