#' Electrical loads of the EUNITE Competition
#' 
#' The EUNITE Competition main dataset composed of a set of univariate time
#' series of half-an-hour electrical loads measured between 1997 and 1998.
#' 
#' The EUNITE Competition proposed the prediction of maximum daily electrical
#' loads based on half-an-hour loads and average daily temperatures of
#' 1997-1998 (\code{\link{EUNITE.Temp}}). The holidays with respect to this
#' period were also provided (\code{\link{EUNITE.Reg}}) and the use of data on
#' average daily temperatures of 1995-1996 was allowed. The dataset present
#' considerable seasonality due to properties of electrical load demand,
#' climate influence and holiday effects, among other reasons. Competitors were
#' asked to predict the 31 values corresponding to the daily maximum electrical
#' loads of January 1999 (\code{\link{EUNITE.Loads.cont}}). The performance
#' evaluation done by the EUNITE Competition was based on the MAPE error and on
#' the MAXIMAL error of prediction found by the competitors.
#' 
#' @name EUNITE.Loads
#' @docType data
#' @format A data frame with 730 observations on the following 48 variables.
#' \describe{ \item{X00.30}{a numeric vector with loads measured in the
#' period 00:00-00:30 of 1997-1998.} \item{X01.00}{a numeric vector
#' with loads measured in the period 00:30-01:00 of 1997-1998.}
#' \item{X01.30}{a numeric vector with loads measured in the period
#' 01:00-01:30 of 1997-1998.} \item{X02.00}{a numeric vector with loads
#' measured in the period 01:30-02:00 of 1997-1998.} \item{X02.30}{a
#' numeric vector with loads measured in the period 02:00-02:30 of 1997-1998.}
#' \item{X03.00}{a numeric vector with loads measured in the period
#' 02:30-03:00 of 1997-1998.} \item{X03.30}{a numeric vector with loads
#' measured in the period 03:00-03:30 of 1997-1998.} \item{X04.00}{a
#' numeric vector with loads measured in the period 03:30-04:00 of 1997-1998.}
#' \item{X04.30}{a numeric vector with loads measured in the period
#' 04:00-04:30 of 1997-1998.} \item{X05.00}{a numeric vector with loads
#' measured in the period 04:30-05:00 of 1997-1998.} \item{X05.30}{a
#' numeric vector with loads measured in the period 05:00-05:30 of 1997-1998.}
#' \item{X06.00}{a numeric vector with loads measured in the period
#' 05:30-06:00 of 1997-1998.} \item{X06.30}{a numeric vector with loads
#' measured in the period 06:00-06:30 of 1997-1998.} \item{X07.00}{a
#' numeric vector with loads measured in the period 06:30-07:00 of 1997-1998.}
#' \item{X07.30}{a numeric vector with loads measured in the period
#' 07:00-07:30 of 1997-1998.} \item{X08.00}{a numeric vector with loads
#' measured in the period 07:30-08:00 of 1997-1998.} \item{X08.30}{a
#' numeric vector with loads measured in the period 08:00-08:30 of 1997-1998.}
#' \item{X09.00}{a numeric vector with loads measured in the period
#' 08:30-09:00 of 1997-1998.} \item{X09.30}{a numeric vector with loads
#' measured in the period 09:00-09:30 of 1997-1998.} \item{X10.00}{a
#' numeric vector with loads measured in the period 09:30-10:00 of 1997-1998.}
#' \item{X10.30}{a numeric vector with loads measured in the period
#' 10:00-10:30 of 1997-1998.} \item{X11.00}{a numeric vector with loads
#' measured in the period 10:30-11:00 of 1997-1998.} \item{X11.30}{a
#' numeric vector with loads measured in the period 11:00-11:30 of 1997-1998.}
#' \item{X12.00}{a numeric vector with loads measured in the period
#' 11:30-12:00 of 1997-1998.} \item{X12.30}{a numeric vector with loads
#' measured in the period 12:00-12:30 of 1997-1998.} \item{X13.00}{a
#' numeric vector with loads measured in the period 12:30-13:00 of 1997-1998.}
#' \item{X13.30}{a numeric vector with loads measured in the period
#' 13:00-13:30 of 1997-1998.} \item{X14.00}{a numeric vector with loads
#' measured in the period 13:30-14:00 of 1997-1998.} \item{X14.30}{a
#' numeric vector with loads measured in the period 14:00-14:30 of 1997-1998.}
#' \item{X15.00}{a numeric vector with loads measured in the period
#' 14:30-15:00 of 1997-1998.} \item{X15.30}{a numeric vector with loads
#' measured in the period 15:00-15:30 of 1997-1998.} \item{X16.00}{a
#' numeric vector with loads measured in the period 15:30-16:00 of 1997-1998.}
#' \item{X16.30}{a numeric vector with loads measured in the period
#' 16:00-16:30 of 1997-1998.} \item{X17.00}{a numeric vector with loads
#' measured in the period 16:30-17:00 of 1997-1998.} \item{X17.30}{a
#' numeric vector with loads measured in the period 17:00-17:30 of 1997-1998.}
#' \item{X18.00}{a numeric vector with loads measured in the period
#' 17:30-18:00 of 1997-1998.} \item{X18.30}{a numeric vector with loads
#' measured in the period 18:00-18:30 of 1997-1998.} \item{X19.00}{a
#' numeric vector with loads measured in the period 18:30-19:00 of 1997-1998.}
#' \item{X19.30}{a numeric vector with loads measured in the period
#' 19:00-19:30 of 1997-1998.} \item{X20.00}{a numeric vector with loads
#' measured in the period 19:30-20:00 of 1997-1998.} \item{X20.30}{a
#' numeric vector with loads measured in the period 20:00-20:30 of 1997-1998.}
#' \item{X21.00}{a numeric vector with loads measured in the period
#' 20:30-21:00 of 1997-1998.} \item{X21.30}{a numeric vector with loads
#' measured in the period 21:00-21:30 of 1997-1998.} \item{X22.00}{a
#' numeric vector with loads measured in the period 21:30-22:00 of 1997-1998.}
#' \item{X22.30}{a numeric vector with loads measured in the period
#' 22:00-22:30 of 1997-1998.} \item{X23.00}{a numeric vector with loads
#' measured in the period 22:30-23:00 of 1997-1998.} \item{X23.30}{a
#' numeric vector with loads measured in the period 23:00-23:30 of 1997-1998.}
#' \item{X24.00}{a numeric vector with loads measured in the period
#' 23:30-24:00 of 1997-1998.} }
#' @seealso \code{\link{EUNITE.Loads.cont}}, \code{\link{EUNITE.Reg}},
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
#' data(EUNITE.Loads)
#' str(EUNITE.Loads)
#' plot(ts(EUNITE.Loads["X24.00"]))
#' 
"EUNITE.Loads"
#> [1] "EUNITE.Loads"