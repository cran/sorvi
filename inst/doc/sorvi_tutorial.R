## ----install, eval=FALSE-------------------------------------------------
#  install.packages("sorvi")

## ----test, eval=FALSE----------------------------------------------------
#  library(sorvi)

## ----locale, eval=FALSE--------------------------------------------------
#  Sys.setlocale(locale="UTF-8")

## ----municipality, message=FALSE, eval=FALSE-----------------------------
#  tab <- get_province_info_wikipedia()
#  head(tab)

## ----translate, message=FALSE, eval=FALSE--------------------------------
#  translations <- load_sorvi_data("translations")
#  head(translations)

## ----municipalityMML, message=FALSE, warning=FALSE, eval=FALSE-----------
#  municipality.info.mml <- get_municipality_info_mml()
#  municipality.info.mml[1:2,]

## ----municipalityStatFi, message=FALSE, warning=FALSE, eval=FALSE--------
#  # Download Statfi municipality data
#  municipality.info.statfi <- get_municipality_info_statfi()
#  
#  # List available information fields for municipalities
#  names(municipality.info.statfi)

## ----postalcodes, message=FALSE, warning=FALSE, eval=FALSE---------------
#  postal.code.table <- get_postal_code_info()
#  head(postal.code.table)

## ----province2, message=FALSE, warning=FALSE, echo=TRUE, eval=FALSE------
#  m2p <- municipality_to_province()
#  head(m2p) # Just show the first ones

## ----province6, message=FALSE, warning=FALSE, echo=TRUE, eval=FALSE------
#  municipality_to_province(c("Helsinki", "Tampere", "Turku"))

## ----province7, message=FALSE, warning=FALSE, echo=TRUE, eval=FALSE------
#  m2p <- municipality_to_province(c("Helsinki", "Tampere", "Turku"), municipality.info.mml)
#  head(m2p)

## ----province3, message=FALSE, echo=TRUE, eval=FALSE---------------------
#  convert_municipality_codes(municipalities = c("Turku", "Tampere"))

## ----province4, message=FALSE, echo=TRUE, eval=FALSE---------------------
#  convert_municipality_codes(ids = c(853, 837))

## ----province5, message=FALSE, echo=TRUE, eval=FALSE---------------------
#  municipality_ids <- convert_municipality_codes()
#  head(municipality_ids) # just show the first entries

## ----hetu, message=FALSE-------------------------------------------------
library(sorvi)
hetu("111111-111C")

## ----hetu2, fig.message=FALSE--------------------------------------------
valid_hetu("010101-0101") # TRUE/FALSE

## ----regressionline, message=FALSE, eval=FALSE, fig.width=10, fig.height=5----
#  library(sorvi)
#  library(plyr)
#  library(RColorBrewer)
#  library(ggplot2)
#  data(iris)
#  p <- regression_plot(Sepal.Length ~ Sepal.Width, iris)
#  print(p)

## ----citation, message=FALSE, eval=FALSE---------------------------------
#  citation("sorvi")

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

