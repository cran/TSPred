% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marimapar.R
\name{marimapar}
\alias{marimapar}
\title{Get parameters of multiple ARIMA models.}
\usage{
marimapar(timeseries, na.action = stats::na.omit, xreg = NULL)
}
\arguments{
\item{timeseries}{A vector, matrix, or data frame which contains a set of
time series used for fitting ARIMA models. Each column corresponds to one
time series.}

\item{na.action}{A function for treating missing values in
\code{timeseries}. The default function is \code{\link[stats]{na.omit}}, which
omits any missing values found in \code{timeseries}.}

\item{xreg}{A vector, matrix, data frame or times series of external
regressors used for fitting all the ARIMA models.  It must have the same
number of rows as \code{TimeSeries}. Ignored if \code{NULL}.}
}
\value{
A list of numeric vectors, each one giving the number of AR, MA,
seasonal AR and seasonal MA coefficients, plus the period and the number of
non-seasonal and seasonal differences of the automatically fitted ARIMA
models. It is also presented the value of the fitted drift constants.
}
\description{
The function returns the parameters of a set of automatically fitted ARIMA
models, including non-seasonal and seasonal orders and drift. Based on
multiple application of the \code{arimapar} function.
}
\details{
See the \code{arimapar} function.
}
\references{
See the \code{arimapar} function.
}
\seealso{
\code{arimapar}, \code{\link{arimapred}}, \code{\link{marimapred}}
}
\keyword{ARIMA}
\keyword{adjustment}
\keyword{automatic}
\keyword{fitting}
\keyword{parameters}
