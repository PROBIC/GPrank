#' @title fixedvarianceKernDiagCompute
#'
#' @description
#' Function for computing the diagonal of the \code{fixedvariance}
#' kernel.
#'
#' @param kern GP kernel structure which contains the
#' fixed variances.
#'
#' @param x One-column matrix which contains the input values
#' for the rows (or the columns) of the kernel.
#'
#' @export
#' @return Return matrix which contains the diagonal elements of the
#' fixed variance kernel.
#'
#' @keywords fixedvariance
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

fixedvarianceKernDiagCompute <-
function (kern, x) {
	k <- kern$fixedvariance
	return (k)
}
