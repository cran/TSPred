#' Continuation dataset of the electrical loads regressors of the EUNITE
#' Competition
#' 
#' A dataset of regressor variables for electrical loads measured in January
#' 1999, providing 31 points beyond the end of the data in
#' \code{\link{EUNITE.Reg}}.
#' 
#' Contains the 31 values of the regressors used for the prediction of the
#' daily maximum electrical loads of January 1999 from
#' \code{\link{EUNITE.Loads}} as demanded by the EUNITE Competition.
#' 
#' @name EUNITE.Reg.cont
#' @docType data
#' @format A data frame with 31 observations on the following 2 variables.
#' \describe{ \item{Holiday}{a numeric vector containing further data
#' of the variable \code{Holiday} in \code{\link{EUNITE.Reg}} relative to
#' January 1999.} \item{Weekday}{a numeric vector containing further
#' data of the variable \code{Weekday} in \code{\link{EUNITE.Reg}} relative to
#' January 1999.} }
#' @seealso \code{\link{EUNITE.Reg}}, \code{\link{EUNITE.Loads}},
#' \code{\link{EUNITE.Temp}}
#' @references B.-J. Chen, M.-W. Chang, and C.-J. Lin, 2004, Load forecasting
#' using support vector Machines: a study on EUNITE competition 2001, IEEE
#' Transactions on Power Systems, v. 19, n. 4 (Nov.), p. 1821-1830.
#' @source EUNITE 1999, Electricity Load Forecast using Intelligent Adaptive
#' Technology: The EUNITE Network Competition. URL:
#' \url{http://www.eunite.org/knowledge/Competitions/1st_competition/1st_competition.htm}.
#' @keywords datasets EUNITE Time Series Competition
#' @examples
#' 
#' data(EUNITE.Reg.cont)
#' str(EUNITE.Reg.cont)
#' 
"EUNITE.Reg.cont"
#> [1] "EUNITE.Reg.cont"