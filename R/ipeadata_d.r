#' The Ipea Most Requested Dataset (daily)
#' 
#' The Institute of Applied Economic Research of Brazil (Ipea) (Ipea, 2017) is
#' a public institution of Brazil that provides support to the federal
#' government with regard to public policies: fiscal, social, and economic.
#' Ipea provides public datasets derived from real economic and financial data
#' of the world.
#' 
#' The \code{ipeadata_d} dataset is provided by Ipea. It comprehends the most
#' requested time series collected in daily rates. The \code{ipeadata_d}
#' dataset comprehend observations of exchange rates (R$/US$), exports/imports
#' prices, interest rates, and more, measured from 1962 to September of 2017.
#' 
#' 
#' The data had missing data removed by the function \code{\link{na.omit}}.
#' 
#' \code{ipeadata_d.cont} provide 30 points beyond the end of the time series
#' in \code{ipeadata_d}. Intended for use as testing set. 
#' 
#' @name ipeadata_d
#' @aliases ipeadata_d ipeadata_d.cont
#' @docType data
#' @format The \code{ipeadata_d} dataset contains 12 time series of 901 to 8154
#' observations.  The 12 time series are provided as the following variables of
#' a data frame.  \describe{ \item{GM366_IBVSP366}{Stock Index: Sao Paulo Stock
#' Exchange - closed - BM&FBovespa.} \item{GM366_ERC366}{Exchange rate - R$ /
#' US$ - commercial - purchase - mean - R$ - Bacen Outras/SGS.}
#' \item{GM366_EREURO366}{Euro area - exchange rate - euro / US$ - mean - Euro
#' - Bacen Outras/SGS.} \item{GM366_ERPV366}{Exchange rate - R$ / US$ -
#' parallel - selling - mean - R$ - Economic value.}
#' \item{GM366_ERV366}{Exchange rate - R$ / US$ - commercial - selling - mean -
#' R$ - Bacen Outras/SGS.} \item{GM366_TJOVER366}{Interest Rate: Over / Selic -
#' (\% p.a.) - Bacen Outras/SGS.} \item{GM366_TJTR366}{Interest rate - TR - (\%
#' p.m.) - Bacen Outras/SGS.} \item{SECEX366_MVTOT366}{Imports - weekly mean -
#' US$ - MDIC/Secex.} \item{SECEX366_XVTOT366}{Exports - weekly mean - US$ -
#' MDIC/Secex.} \item{JPM366_EMBI366}{EMBI + Risco-Brasil - JP Morgan.}
#' \item{BM366_TJOVER366}{Interest rate - Selic - fixed by Copom - (\% p.a.) -
#' Bacen/Boletim/M. Finan..} \item{GM366_TJOVERV366}{Interest Rate: Over /
#' Selic - Ipea.} }
#' @seealso \code{\link{ipeadata_m}} ~
#' @references Ipea, Ipeadata. Macroeconomic and regional data, Technical
#' Report, \url{http://www.ipeadata.gov.br}, 2017. 
#' @source Ipea, Ipeadata. Macroeconomic and regional data, Technical Report,
#' \url{http://www.ipeadata.gov.br}, 2017. The "Most request series" section
#' and filtered by "Frequency" equal to "Daily". 
#' @keywords datasets ipeadata time teries
#' @examples
#' 
#' data(ipeadata_d)
#' str(ipeadata_d)
#' plot(ts(ipeadata_d[1]))
#' 
"ipeadata_d"
#> [1] "ipeadata_d"