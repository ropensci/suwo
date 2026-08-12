# Pre-compiled vignettes that depend on API key
# the original Rmd file is not included in the package,
# but is available in the GitHub repository at ./testing/suwo_vignette.Rmd
# modify and test ./testing/suwo_vignette.Rmd and then save it as
# vignettes/suwo.Rmd.orig
# save macaulay csvs in ./vignettes/
knitr::knit("vignettes/suwo.Rmd.orig", "vignettes/suwo.Rmd")
