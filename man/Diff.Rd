% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff.r
\name{Diff}
\alias{Diff}
\alias{Diff.rev}
\title{Differencing Transformation}
\usage{
Diff(
  x,
  lag = ifelse(type == "simple", 1, stats::frequency(x)),
  differences = NULL,
  type = c("simple", "seasonal"),
  ...
)

Diff.rev(
  x,
  lag = ifelse(type == "simple", 1, stats::frequency(x)),
  differences = 1,
  xi,
  type = c("simple", "seasonal"),
  addinit = TRUE
)
}
\arguments{
\item{x}{A numeric vector or univariate time series containing the values to
be differenced.}

\item{lag}{Integer indicating the lag parameter. Default set to \code{1} if
\code{type = "simple"}, or \code{frequency(x)} if \code{type = "seasonal"}.}

\item{differences}{Integer representing the order of the difference. If
\code{NULL}, the order of the difference is automatically selected using
\code{\link[forecast]{ndiffs}} (if \code{type = "simple"}) or
\code{\link[forecast]{nsdiffs}} (if \code{type = "seasonal"}) from the
\code{forecast} package.}

\item{type}{Character string. Indicates if the function should perform
simple or seasonal differencing.}

\item{...}{Additional arguments passed to \code{\link[forecast]{ndiffs}} (if
\code{type = "simple"}) or \code{\link[forecast]{nsdiffs}} (if \code{type =
"seasonal"}) from the \code{forecast} package.}

\item{xi}{Numeric vector or time series containing the initial values for
the integrals. If missing, zeros are used.}

\item{addinit}{If \code{FALSE}, the reverse transformed time series does not 
contain \code{xi}. Default set to \code{TRUE}.}
}
\value{
\code{x} if \code{differences} is automatically selected, and is not
		set as greater than \code{0}. Same as \code{\link{diff}} otherwise.
}
\description{
The \code{Diff()} function returns a simple or seasonal differencing
transformation of the provided time series. \code{Diff.rev()} reverses the
transformation. Wrapper functions for \code{\link{diff}} and
\code{\link[stats]{diffinv}} of the \code{stats} package, respectively.
}
\examples{

data(CATS)
d <- Diff(CATS[,1], differences = 1)
x <- Diff.rev(as.vector(d), differences = attributes(d)$differences, xi = attributes(d)$xi)
all(round(x,4)==round(CATS[,1],4))

}
\references{
R.J. Hyndman and G. Athanasopoulos, 2013, Forecasting:
principles and practice. OTexts.

R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and Its
Applications: With R Examples. 3rd ed. 2011 edition ed. New York, Springer.
}
\seealso{
Other transformation methods: 
\code{\link{LogT}()},
\code{\link{WaveletT}()},
\code{\link{emd}()},
\code{\link{mas}()},
\code{\link{mlm_io}()},
\code{\link{outliers_bp}()},
\code{\link{pct}()},
\code{\link{train_test_subset}()}
}
\author{
Rebecca Pontes Salles
}
\concept{transformation methods}
\keyword{differencing}
\keyword{series}
\keyword{time}
\keyword{transform}
