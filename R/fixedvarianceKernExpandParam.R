#' @title fixedvarianceKernExpandParam
#'
#' @description
#' Function for expanding the parameters of the \code{fixedvariance} kernel.
#' As the parameters of the \code{fixedvariance} kernel are fixed, and thus
#' are not optimized, their values remain the same. This function is
#' written to comply with the \code{gptk} package.
#'
#' @param kern GP kernel structure which contains the
#' fixed variances.
#'
#' @param params Parameter values with which the kernel will be
#' updated.
#'
#' @export
#' @return Return kernel with the updated parameter values.
#'
#' @keywords fixedvariance
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

fixedvarianceKernExpandParam <-
function(kern,params) {
	kern=kern
	return(kern)	
}
