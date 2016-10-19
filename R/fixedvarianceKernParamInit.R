#' @title fixedvarianceKernParamInit
#'
#' @description
#' Function for initializing the parameters of the \code{fixedvariance}
#' kernel. The parameters of the \code{fixedvariance} kernel are
#' specified when constructing the model and always kept fixed.
#'
#' @param kern GP kernel structure.
#'
#' @export
#' @return Return kernel with the fixed variances in its diagonal. 
#'
#' @keywords fixedvariance 
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

fixedvarianceKernParamInit <-
function(kern) {
	kern$fixedvariance=kern$options$variance
	kern$fixedvariance_times=kern$options$input
	kern$use_fixedvariance=1
	kern$nParams=0
	kern$isStationary=TRUE
	return(kern)
}
