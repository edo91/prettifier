% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prettify_string.R
\name{prettify_string}
\alias{prettify_string}
\title{Prettify strings}
\usage{
prettify_string(x, ...)
}
\arguments{
\item{x}{A string or a vector of strings.}

\item{...}{Custom replacement. See examples.}
}
\value{
Returns a clean x
}
\description{
Remove:
\itemize{
\item punctuation,
\item special characters,
\item spaces and tabs,
\item accents
}

Force everything to lower
}
\examples{

# remove all symbols and accents
x <- "a1~!#$â‚¬\%^&*|__+:____()[]{}<>?,./;:'-=Ã²Ã¬Ã Ã¨Ã©Ã¹ dASFGHRV"
y <- c("a1~!#$â‚¬\%^&*|__+:____()[]{}<>?,./;:'-=Ã²Ã¬Ã Ã¨Ã©Ã¹ dASFGHRV",
       "CIAAAOOOO")

prettify_string(x)
prettify_string(y)

# custom replacement
prettify_string(x = "5\%_in_â‚¬")
prettify_string(x = "5\%_in_â‚¬", "\%" = "perc")
prettify_string(x = "5\%_in_â‚¬", "\%" = "perc", "â‚¬" = "eur")


}
