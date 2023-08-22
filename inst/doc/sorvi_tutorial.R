## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("sorvi")

## ----install2, eval=FALSE-----------------------------------------------------
#  library(remotes)
#  remotes::install_github("ropengov/sorvi")

## ----test, message=FALSE, warning=FALSE, eval=TRUE----------------------------
library(sorvi)

## ----example_download_stats, eval=FALSE---------------------------------------
#  df <- cran_downloads(pkgs = "eurostat", sum = "by_year", use.cache = FALSE)
#  df

## ----example_visualize, eval=FALSE--------------------------------------------
#  packages <- c("eurostat", "giscoR", "sotkanet", "geofi", "sweidnumbr")
#  plot <- cran_downloads(pkgs = packages, sum = "total", output = "plot", use.cache = FALSE)
#  plot

## ----example_map--------------------------------------------------------------
library(ggplot2)
map1931 <- get_municipalities(year = 1931)
ggplot(map1931) + geom_sf()

## ----citation, message=FALSE, eval=TRUE---------------------------------------
citation("sorvi")

## ----sessioninfo, message=FALSE, warning=FALSE--------------------------------
sessionInfo()

