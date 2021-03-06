% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/hetu.R
\name{hetu}
\alias{hetu}
\title{Extract information from Finnish personal identification numbers (hetu)}
\usage{
hetu(hetu, extract = NULL)
}
\arguments{
\item{hetu}{Finnish personal identification number as a character vector,
or vector of identification numbers as a character vectors}

\item{extract}{Extract only selected part of the information.
   Valid values are "\code{hetu}", "\code{gender}", "\code{personal.number}",
   "\code{checksum}", "\code{date}", "\code{day}", "\code{month}",
   "\code{year}", "\code{century.char}".
   If \code{NULL} (default), returns all information.}
}
\value{
Finnish personal identification number data.frame,
        or if extract parameter is set, the requested part of the
	   information as a vector. Returns \code{NA} if the given character
	   vector is not a valid Finnish personal identification number.
\item{hetu}{Finnish personal identification number as a character vector.}
\item{gender}{Gender of the person as a character vector ("Male" or "Female").}
\item{personal.number}{Personal number part of the identification number.}
\item{checksum}{Checksum for the personal identification number.}
\item{date}{Birthdate.}
\item{day}{Day of the birthdate.}
\item{month}{Month of the birthdate.}
\item{year}{Year of the birthdate.}
\item{century.char}{Century character of the birthdate:
                    + (1800), - (1900) or A (2000). }
}
\description{
Extract information from Finnish personal identification numbers (hetu)
}
\examples{
hetu("111111-111C")
hetu("111111-111C")$date
hetu("111111-111C")$gender
# Same as previous, but using extract argument
hetu("111111-111C", extract="gender")

# Process a vector of hetu's
hetu(c("010101-0101", "111111-111C"))

# Process a vector of hetu's and extract gender information from each
hetu(c("010101-0101", "111111-111C"), extract="gender")
}
\author{
Jussi Paananen \email{louhos@googlegroups.com}
}
\seealso{
\code{\link{valid_hetu}} For validating Finnish personal
	    identification numbers.
}

