#' @title Setting limits for the y-axis
#'
#' @description
#' Function for setting decent y-axis limits.
#' 
#' @param Y Matrix containing observed values; items in rows, time points in columns.
#' @param V Matrix containing variances; items in rows, time points in columns.
#' @param predY Matrix containing predicted values; items in rows, time points in columns.
#' @param predV Matrix containing variances of the predicted values; items in rows, time points in columns.
#' @param minRng Minimum range for the y axis in the plot. Default value is set to 0.5.
#'
#' @export
#' @return Return y-axis limits.
#'
#' @examples
#' Y=matrix(c(1,2,3,4,5,6),2,3)
#' V=0.1*Y
#' predY=matrix(c(1,2,3,4,5,6),2,3)
#' predV=0.1*predY
#' y_lims=getYlimits(Y,V,predY,predV)
#'
#' @keywords axis limits
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 
getYlimits <-
function(Y,V,predY=NULL,predV=NULL,minRng=0.5) {
  
  ylimits=c(min(Y-2*sqrt(V),na.rm=TRUE),max(Y+2*sqrt(V),na.rm=TRUE))
  if (!is.null(predY) & !is.null(predV)) {
    ypredlimits=c(min(predY-2*sqrt(predV),na.rm=TRUE),max(predY+2*sqrt(predV),na.rm=TRUE))
    ylimits=range(ylimits,ypredlimits)
  }
  rng=diff(range(ylimits))
  if (rng<minRng) {
    ylimits=c(min(ylimits)-(minRng-rng)/2,max(ylimits)+(minRng-rng)/2)
  }
  
  return(ylimits)
  
}		
