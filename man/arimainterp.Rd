% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arimainterp.R
\name{arimainterp}
\alias{arimainterp}
\title{Interpolation of unknown values using automatic ARIMA fitting and prediction}
\usage{
arimainterp(
  TimeSeries,
  n.ahead,
  extrap = TRUE,
  xreg = NULL,
  newxreg = NULL,
  se.fit = FALSE
)
}
\arguments{
\item{TimeSeries}{A matrix, or data frame which contains a set of time
series used for fitting ARIMA models. Each column corresponds to one time
series. Each time series in \code{TimeSeries} is assumed to be a sequence of
known values of the single time series that intercalates blocks of unknown
values. The time series values in column 1 are lagged values of the ones in
column 2, and the values in these two columns are assumed to be intercalated
by the first block of N unknown values to be predicted. This is also valid
for columns 2 and 3, and so forth.}

\item{n.ahead}{A numeric value (N) with the number of consecutive unknown
values of each block which is to be predicted of \code{TimeSeries}, that is,
the length of the blocks of N unknown values.}

\item{extrap}{A Boolean parameter which defines whether one of the blocks of
N unknown values to be predicted follows the last sequence of known values
in \code{TimeSeries}. If \code{extrap} is \code{TRUE}, the last block of N
unknown values will be extrapolated from the last time series in
\code{TimeSeries}.}

\item{xreg}{A list of vectors, matrices, data frames or times series of
external regressors used for fitting the ARIMA models. The first component
of the list contains external regressors for the first time series in
\code{TimeSeries} and therefore must have the same number of rows as this
respective time series. This is also valid for the second component, and so
forth. Ignored if \code{NULL}.}

\item{newxreg}{A list of vectors, matrices, data frames or times series with
further values of \code{xreg} to be used for prediction of the blocks of N
unknown values. Each component of the list must have at least \code{n.ahead}
rows. Ignored if \code{NULL}.}

\item{se.fit}{If \code{se.fit} is \code{TRUE}, the standard errors of the
predictions are returned.}
}
\value{
A vector of time series of predictions, or if \code{se.fit} is
\code{TRUE}, a vector of lists, each one with the components \code{pred},
the predictions, and \code{se}, the estimated standard errors. Both
components are time series. See the \code{\link{predict.Arima}} function in
the stats package and the function \code{\link{arimapred}}.
}
\description{
The function predicts nonconsecutive blocks of N unknown values of a single
time series using the \code{\link{arimapred}} function and an interpolation
approach.
}
\details{
In order to avoid error accumulation, when possible, the function provides
the separate prediction of each half of the blocks of unknown values using
their past and future known values, respectively. If \code{extrap} is
\code{TRUE}, this strategy is not possible for the last of the blocks of
unknown values, for whose prediction the function uses only its past values.
By default the function omits any missing values found in \code{TimeSeries}.
}
\examples{

\donttest{
data(CATS)
arimainterp(CATS[,c(2:3)],n.ahead=20,extrap=TRUE)
}

}
\references{
H. Cheng, P.-N. Tan, J. Gao, and J. Scripps, 2006,
"Multistep-Ahead Time Series Prediction", In: W.-K. Ng, M. Kitsuregawa, J.
Li, and K. Chang, eds., Advances in Knowledge Discovery and Data Mining,
Springer Berlin Heidelberg, p. 765-774.
}
\seealso{
\code{\link{arimapred}}, \code{\link{marimapred}}
}
\author{
Rebecca Pontes Salles
}
\keyword{ARIMA}
\keyword{adjustment}
\keyword{automatic}
\keyword{fitting}
\keyword{interpolation}
\keyword{prediction}
