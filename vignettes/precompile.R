# Pre-compiled vignettes that depend on API key
# the original Rmd file is not included in the package,
# but is available in the GitHub repository at ./testing/suwo_v2.Rmd
# save macaulay csvs in ./vignettes/
knitr::knit("vignettes/suwo.Rmd.orig", "vignettes/suwo.Rmd")
