#' @title fixedvarianceKernGradient
#'
#' @description
#' Function for computing the gradient of the \code{fixedvariance} kernel.
#' As the parameters of the \code{fixedvariance} kernel are fixed, the 
#' gradient equals zero. This function is written to comply with
#' the \code{gptk} package.
#'
#' @param kern GP kernel structure which contains the
#' fixed variances.
#' @param x x
#' @param x2 x2
#' @param covGrad covGrad
#'
#' @export
#' @return Return value for the gradient of fixedvariance kernel.
#'
#' @keywords fixedvariance
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#'

fixedvarianceKernGradient <-
function(kern,x,x2,covGrad) {	
	g=0
	return(g)
}
