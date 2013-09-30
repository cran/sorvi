# This file is a part of the sorvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2013 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



#' Get PC Axis data with custom preprocessing for PC Axis 
#' files from Statistics Finland (Tilastokeskus) http://www.stat.fi/
#'
#' Arguments:
#'  @param px PC Axis object, or its URL.
#'
#' Returns:
#'  @return PC Axis object.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{louhos@@googlegroups.com}
#' @examples 
#' # px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")
#' @keywords utilities

GetPXTilastokeskus <- function (px) {

  # TODO: lisaa tarkemmat ja kattavammat esikasittelyt
  # Talla saa luetuksi CSV-versioita:
  # tab <- read.csv(gsub("\\.px", "\\.csv", url2012), encoding = "latin1", as.is = T, colClasses = 'character', sep = ";"); colnames(tab) <- tab[1,]; tab <- tab[-c(1,2),]; colnames(tab)[[1]] <- "Ehdokas"; df <- melt(tab, id = "Ehdokas"); colnames(df) <- c("Ehdokas", "Äänestystiedot", "dat"); df <- df[, c(2,1,3)]

  # If URL is given, read the data into PX object
  if (is.url(px)) {
    message(paste("Reading Tilastokeskus data from ", px))
    px <- sorvi::read.px(px)	 
  }

  # Convert to data.frame 
  if (class(px) == "px") { 
    px <- as.data.frame(px) 
  }

  # Preprocess field names
  fields <- c("Alue", "Kunta")
  for (nam in intersect(fields, colnames(px))) {
    px[[nam]] <- sapply(px[[nam]], function (x) {strsplit(as.character(x), " - ")[[1]][[1]]})
  }    

  px

}


