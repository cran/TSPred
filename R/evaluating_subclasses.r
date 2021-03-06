#' Prediction/modeling quality metrics
#'
#' Constructors for the \code{evaluating} class representing a time series prediction
#' or modeling fitness quality evaluation based on particular metrics.
#'
#' @section Error metrics:
#' 	MSE_eval: Mean Squared Error.
#'
#' @return An object of class \code{evaluating}.
#' @author Rebecca Pontes Salles
#' @family constructors
#'
#' @keywords quality evaluation metric
#' 
#' @rdname quality_metrics
#' @export MSE_eval
MSE_eval <- function(){
  error(eval_func=TSPred::MSE, eval_par=NULL, method="Mean Squared Error", subclass="MSE")
}

#Subclass NMSE
#' @rdname quality_metrics
#' @section Error metrics:
#' 	NMSE_eval: Normalised Mean Squared Error.
#' @param eval_par List of named parameters required by \code{\link{NMSE}} such as \code{train.actual}.
#' @export
NMSE_eval <- function(eval_par=list(train.actual=NULL)){
  error(eval_func=TSPred::NMSE, eval_par=eval_par, method="Normalised Mean Squared Error", subclass="NMSE")
}

#Subclass RMSE
#' @rdname quality_metrics
#' @section Error metrics:
#' 	RMSE_eval: Root Mean Squared Error.
#' @export
RMSE_eval <- function(){
  error(eval_func=ModelMetrics::rmse, eval_par=NULL, method="Root Mean Squared Error", subclass="RMSE")
}

#Subclass MAPE
#' @rdname quality_metrics
#' @section Error metrics:
#' 	MAPE_eval: Mean Absolute Percentage Error.
#' @export
MAPE_eval <- function(){
  error(eval_func=TSPred::MAPE, eval_par=NULL, method="Mean Absolute Percentage Error", subclass="MAPE")
}

#Subclass sMAPE
#' @rdname quality_metrics
#' @section Error metrics:
#' 	sMAPE_eval: Symmetric Mean Absolute Percentage Error.
#' @export
sMAPE_eval <- function(){
  error(eval_func=TSPred::sMAPE, eval_par=NULL, method="Symmetric Mean Absolute Percentage Error", subclass="sMAPE")
}

#Subclass MAXError
#' @rdname quality_metrics
#' @section Error metrics:
#' 	MAXError_eval: Maximal Error.
#' @export
MAXError_eval <- function(){
  error(eval_func=TSPred::MAXError, eval_par=NULL, method="Maximal Error", subclass="MAXError")
}

#Subclass AIC
#' @rdname quality_metrics
#' @section Fitness criteria:
#' 	AIC_eval: Akaike's Information Criterion.
#' @export
AIC_eval <- function(){
  fitness(eval_func=stats::AIC, eval_par=NULL, method="Akaike's Information Criterion", subclass="AIC")
}

#Subclass BIC
#' @rdname quality_metrics
#' @section Fitness criteria:
#' 	BIC_eval: Schwarz's Bayesian Information Criterion.
#' @export
BIC_eval <- function(){
  fitness(eval_func=stats::BIC, eval_par=NULL, method="Schwarz's Bayesian Information Criterion", subclass="BIC")
}

#Subclass AICc
#' @rdname quality_metrics
#' @section Fitness criteria:
#' 	AICc_eval: Second-order Akaike's Information Criterion.
#' @export
AICc_eval <- function(){
  fitness(eval_func=MuMIn::AICc, eval_par=NULL, method="Second-order Akaike's Information Criterion", subclass="AICc")
}

#Subclass logLik
#' @rdname quality_metrics
#' @section Fitness criteria:
#' 	LogLik_eval: Log-Likelihood.
#' @export
LogLik_eval <- function(){
  fitness(eval_func=stats::logLik, eval_par=NULL, method="Log-Likelihood", subclass="LogLik")
}