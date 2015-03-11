marimapred <-
function(TimeSeries,TimeSeriesCont=NULL,n.ahead=NULL,na.action=na.omit,xreg=NULL,newxreg=NULL,se.fit=FALSE,plot=FALSE,range.p=0.2,ylab=NULL,xlab=NULL,main=NULL){
    if(!is.null(TimeSeriesCont)) {
        if(!is.null(ylab)) {
            Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, MoreArgs = list(xreg=xreg, newxreg=newxreg, se.fit=se.fit, n.ahead=n.ahead, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
        }
        else {
            Predictions <- mapply(arimapred, TimeSeries, TimeSeriesCont, ylab=colnames(TimeSeries), MoreArgs = list(xreg=xreg, newxreg=newxreg, se.fit=se.fit, n.ahead=n.ahead, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
        }
    }
    else{
        if(!is.null(ylab)) {
            Predictions <- mapply(arimapred, TimeSeries, MoreArgs = list(timeseries.cont=TimeSeriesCont, xreg=xreg, newxreg=newxreg, se.fit=se.fit, n.ahead=n.ahead, na.action=na.action, range.p=range.p, plot=plot, ylab=ylab, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
        }
        else {
            Predictions <- mapply(arimapred, TimeSeries, ylab=colnames(TimeSeries), MoreArgs = list(timeseries.cont=TimeSeriesCont, xreg=xreg, newxreg=newxreg, se.fit=se.fit, n.ahead=n.ahead, na.action=na.action, range.p=range.p, plot=plot, xlab=xlab, main=main), SIMPLIFY = TRUE, USE.NAMES = TRUE)
        }
    }
    return (Predictions)
}
