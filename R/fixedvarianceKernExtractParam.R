#' @title fixedvarianceKernExtractParam
#'
#' @description
#' Function for extracting the parameters of the \code{fixedvariance} kernel.
#' As the parameters of the \code{fixedvariance} kernel are fixed, the kernel
#' has no parameters to be optimized. Hence, this function returns a
#' NULL vector. This function is written to comply with the \code{gptk} package.
#'
#' @param kern GP kernel structure which contains the
#' fixed variances.
#' @param only.values Default set to TRUE.
#' @param untransformed.values Default set to TRUE.
#'
#' @export
#' @return Return parameters of the kernel.
#'
#' @keywords fixedvariance
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#'

fixedvarianceKernExtractParam <-
function (kern, only.values=TRUE, untransformed.values=TRUE) {
	params <- c()	
	return (params)
}

