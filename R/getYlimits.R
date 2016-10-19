#' @title Setting limits for the y-axis
#'
#' @description
#' Function for setting decent y-axis limits.
#' 
#' @param Y Matrix containing observed values; items in rows, time points in columns.
#' @param V Matrix containing variances; items in rows, time points in columns.
#' @param minRng Minimum range for the y axis in the plot. Default value is set to 0.5.
#'
#' @export
#' @return Return y-axis limits.
#'
#' @examples
#' Y=matrix(c(1,2,3,4,5,6),2,3)
#' V=0.1*Y
#' y_lims=getYlimits(Y,V)
#'
#' @keywords axis limits
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 
getYlimits <-
function(Y,V,minRng=0.5) {
		
	ylimits=c(min(Y-2*sqrt(V),na.rm=TRUE),max(Y+2*sqrt(V),na.rm=TRUE))
	rng=diff(range(ylimits))
	if (rng<minRng) {
		ylimits=c(min(ylimits)-(minRng-rng)/2,max(ylimits)+(minRng-rng)/2)
	}

	return(ylimits)

}		


