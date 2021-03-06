% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tspred.r
\name{benchmark}
\alias{benchmark}
\alias{benchmark.tspred}
\title{Benchmarking a time series prediction process}
\usage{
benchmark(obj, ...)

\method{benchmark}{tspred}(obj, bmrk_objs, rank.by = c("MSE"), ...)
}
\arguments{
\item{obj}{An object of class \code{\link{tspred}} defining a particular time series prediction process.}

\item{...}{Ignored}

\item{bmrk_objs}{A list of objects of class \code{\link{tspred}} to be compared against \code{obj}.}

\item{rank.by}{A vector of the given names of the metrics that should base the ranking.}
}
\value{
A list containing:
\item{rank}{A data.frame with the ranking of metrics computed for the benchmarked \code{\link{tspred}} objects.}
\item{ranked_tspred_objs}{A list of the benchmarked \code{\link{tspred}} objects ordered according to the produced rank.}
}
\description{
\code{benchmark} is a generic function for benchmarking results based on particular metrics.
The function invokes particular \emph{methods} which
depend on the class of the first argument.
}
\details{
The function \code{\link{benchmark.tspred}} benchmarks a time series prediction process
defined by a \code{\link{tspred}} object based on a particular metric. The metrics resulting
from its execution are compared against the ones produced by other time series prediction
processes (defined in a list of \code{\link{tspred}} objects).
}
\examples{
\donttest{
#Obtaining objects of the processing class
proc1 <- subsetting(test_len=20)
proc2 <- BoxCoxT(lambda=NULL)
proc3 <- WT(level=1, filter="bl14")

#Obtaining objects of the modeling class
modl1 <- ARIMA()

#Obtaining objects of the evaluating class
eval1 <- MSE_eval()
eval2 <- MAPE_eval()

#Defining a time series prediction process
tspred_1 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2,
                                   WT=proc3),
                   modeling=modl1,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2)
                  )
summary(tspred_1)

#Obtaining objects of the processing class
proc4 <- SW(window_len = 6)
proc5 <- MinMax()

#Obtaining objects of the modeling class
modl2 <- NNET(size=5,sw=proc4,proc=list(MM=proc5))

#Defining a time series prediction process
tspred_2 <- tspred(subsetting=proc1,
                   processing=list(BCT=proc2,
                                   WT=proc3),
                   modeling=modl2,
                   evaluating=list(MSE=eval1,
                                   MAPE=eval2)
                  )
summary(tspred_2)

data("CATS")
data <- CATS[3]

tspred_1_run <- workflow(tspred_1,data=data,prep_test=TRUE,onestep=TRUE)
tspred_2_run <- workflow(tspred_2,data=data,prep_test=TRUE,onestep=TRUE)

b <- benchmark(tspred_1_run,list(tspred_2_run),rank.by=c("MSE"))
}

}
\seealso{
[tspred()] for defining a particular time series prediction process.
}
\author{
Rebecca Pontes Salles
}
\concept{benchmark}
\keyword{benchmark}
\keyword{evaluate}
\keyword{model}
\keyword{prediction}
\keyword{preprocess}
\keyword{tspred}
