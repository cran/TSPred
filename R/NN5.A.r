#' Dataset A of the NN5 Competition 
#' 
#' The NN5 Competition dataset composed of daily time series originated from
#' the observation of daily withdrawals at 111 randomly selected different cash
#' machines at different locations within England. 
#' 
#' The NN5 Competition's Dataset A contains 111 different daily time series.
#' Each of these time series possesses 735 observations, and may present
#' missing data. The time series also show different patterns of single or
#' multiple overlying seasonal properties. Each competitor in NN5 was asked to
#' predict the next 56 corresponding observations of each times series
#' (\code{\link{NN5.A.cont}}). The performance evaluation done by NN5
#' Competition was based on the mean SMAPE error of prediction found by the
#' competitors across all time series. 
#' 
#' @name NN5.A
#' @docType data
#' @format A data frame with 735 observations on the following 111 variables.
#' \describe{ \item{NN5.001}{a numeric vector containing observations
#' of a univariate time series.} \item{NN5.002}{a numeric vector
#' containing observations of a univariate time series.}
#' \item{NN5.003}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.004}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.005}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.006}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.007}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.008}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.009}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.010}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.011}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.012}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.013}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.014}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.015}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.016}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.017}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.018}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.019}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.020}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.021}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.022}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.023}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.024}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.025}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.026}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.027}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.028}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.029}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.030}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.031}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.032}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.033}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.034}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.035}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.036}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.037}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.038}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.039}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.040}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.041}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.042}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.043}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.044}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.045}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.046}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.047}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.048}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.049}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.050}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.051}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.052}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.053}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.054}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.055}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.056}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.057}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.058}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.059}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.060}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.061}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.062}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.063}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.064}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.065}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.066}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.067}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.068}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.069}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.070}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.071}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.072}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.073}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.074}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.075}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.076}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.077}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.078}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.079}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.080}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.081}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.082}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.083}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.084}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.085}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.086}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.087}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.088}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.089}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.090}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.091}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.092}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.093}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.094}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.095}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.096}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.097}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.098}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.099}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.100}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.101}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.102}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.103}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.104}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.105}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.106}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.107}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.108}{a numeric vector containing observations of a
#' univariate time series.} \item{NN5.109}{a numeric vector containing
#' observations of a univariate time series.} \item{NN5.110}{a numeric
#' vector containing observations of a univariate time series.}
#' \item{NN5.111}{a numeric vector containing observations of a
#' univariate time series.} }
#' @seealso \code{\link{NN5.A.cont}} ~
#' @references S.F. Crone, 2008, Results of the NN5 time series forecasting
#' competition. Hong Kong, Presentation at the IEEE world congress on
#' computational intelligence. WCCI'2008. 
#' @source NN5 2008, The NN5 Competition: Forecasting competition for
#' artificial neural networks and computational intelligence. URL:
#' \url{http://www.neural-forecasting-competition.com/NN5/index.htm}. 
#' @keywords datasets NN5 Time Series Competition
#' @examples
#' 
#' data(NN5.A)
#' str(NN5.A)
#' plot(ts(NN5.A["NN5.111"]))
#' 
"NN5.A"
#> [1] "NN5.A"