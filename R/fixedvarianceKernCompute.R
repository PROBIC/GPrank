#' @title fixedvarianceKernCompute
#'
#' @description
#' Function for computing the fixed variance kernel.
#'
#' @param kern GP kernel structure which contains the
#' fixed variances to be included in the \code{fixedvariance}
#' kernel.
#'
#' @param x One-column matrix which contains the input
#' values for the rows of the kernel.
#'
#' @param x2 One-column matrix which contains the input
#' values for the columns of the kernel. If missing,
#' a square matrix is created with \code{x} on both 
#' rows and columns.
#'
#' @export
#' @return Return kernel with the fixed variances in the diagonal,
#' and zeros in the off-diagonal elements.
#'
#' @import gptk
#' @import matrixStats
#'
#' @importFrom grDevices dev.off pdf png 
#' @importFrom graphics axis box frame lines par plot points polygon title legend
#' @importFrom utils head read.table tail write.table
#' @importFrom stats var
#'
#' @keywords fixedvariance 
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

fixedvarianceKernCompute <-
function (kern, x, x2) {
	if ( nargs()<3 ) {
		k <- diag(kern$fixedvariance[,1])
	} else {
		x1dim <- dim(as.array(x))[1]
		x2dim <- dim(as.array(x2))[1]
		k <- matrix(0, nrow=x1dim, ncol=x2dim)
	}
	return (k)
}

