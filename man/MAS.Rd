% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MAS.r
\name{mas}
\alias{mas}
\alias{mas.rev}
\title{Moving average smoothing}
\usage{
mas(x, order, ...)

mas.rev(xm, xi, order, addinit = TRUE)
}
\arguments{
\item{x}{A numeric vector or univariate time series.}

\item{order}{Order of moving average smoother. If \code{NULL}, it is 
automatically selected by \code{\link{fittestMAS}}.}

\item{...}{Additional arguments passed to \code{\link{fittestMAS}}.}

\item{xm}{A numeric vector or univariate time series that has been moving average
smoothed. Possibly returned by \code{mas()}.}

\item{xi}{Initial \code{order-1} values/observations used for reverse
smoothing. First \code{order-1} known non-transformed values used to
recursively obtain the original series. 
By default, \code{mas()} returns \code{xi} as an attribute.}

\item{addinit}{If \code{TRUE}, \code{xi} is included in the return.}
}
\value{
Numerical time series of length \code{length(x)-order+1} containing
the simple moving average smoothed values.
}
\description{
The \code{mas()} function returns a simple moving average smoother of the
provided time series. \code{mas.rev()} reverses the
transformation(smoothing) process.
}
\details{
The moving average smoother transformation is given by \deqn{(1/k) * (
x[t] + x[t+1] + ... + x[t+k-1] )} where \code{k=order}, \code{t} assume
values in the range \code{1:(n-k+1)}, and \code{n=length(x)}. See also the
\code{\link[forecast]{ma}} of the \code{forecast} package.
}
\examples{

data(CATS)

m <- mas(CATS[,1],order=5)
\donttest{
#automatically select order of moving average
m <- mas(CATS[,1],order=NULL,h=20)
}

x <- mas.rev(m, attributes(m)$xi, attributes(m)$order)

all(round(x,4)==round(CATS[,1],4))

}
\references{
R.H. Shumway and D.S. Stoffer, 2010, Time Series Analysis and
Its Applications: With R Examples. 3rd ed. 2011 edition ed. New York,
Springer.
}
\seealso{
Other transformation methods: 
\code{\link{Diff}()},
\code{\link{LogT}()},
\code{\link{WaveletT}()},
\code{\link{emd}()},
\code{\link{mlm_io}()},
\code{\link{outliers_bp}()},
\code{\link{pct}()},
\code{\link{train_test_subset}()}
}
\author{
Rebecca Pontes Salles
}
\concept{transformation methods}
\keyword{average}
\keyword{moving}
\keyword{series}
\keyword{smoother}
\keyword{time}
\keyword{transform}
