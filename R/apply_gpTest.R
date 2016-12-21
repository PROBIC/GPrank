#' @title Performing Gaussian process test
#'
#' @description
#' Function for fitting time-dependent and time-independent GP models
#' and computing the Bayes factors.
#'
#' @param x One-column matrix which contains the input
#' values, i.e., time points for GP models.
#' The values given in this vector are used in GP model, so if any
#' transformation is needed, remember to transform them now.
#' @param y One-column matrix which contains the observed
#' values at the corresponding time points given in \code{x} vector. 
#' @param v One-column matrix which contains the fixed
#' variances at the corresponding time points given in \code{x} vector,
#' with the corresponding observations given in \code{y} vector. 
#' @param nullModelKernelTypes Character vector which contains the type of
#' the kernels which will be used in the null, i.e.,
#' time-independent GP model. Default is c("white","fixedvariance").
#' @param modelKernelTypes Character vector which contains the type of
#' the kernels which will be used in the time-dependent 
#' GP model. Default is c("rbf","white","fixedvariance").
#' Kernel types: \code{'rbf'}, \code{'white'}, \code{'fixedvariance'}.
#' Note that the lower bound for the length scale parameter of rbf kernel
#' is set to the minimum distance between consecutive time points in order
#' to mitigate potential overfitting problems.
#' @param y_fitted Logical variable indicating whether the fitted y values
#' at the observed time points will be given or not. Default is set to FALSE.
#'
#' @export
#' @return Return list which contains logged Bayes factors (logBF) and the fitted
#' GP models (nullModel & model) with the specified kernel structures. If y_fitted 
#' is set to TRUE, fitted y values of the model are returned in y_fitted as
#' the list element.
#'
#' @examples
#' x=as.matrix(seq(1,10))
#' y=as.matrix(sin(x))
#' v=as.matrix(runif(10,0,0.5))
#' nullModelKernelTypes=c("white","fixedvariance")
#' modelKernelTypes=c("rbf","white","fixedvariance")
#' test_result=apply_gpTest(x,y,v,nullModelKernelTypes,modelKernelTypes,y_fitted=TRUE)
#' null_model=test_result$nullModel
#' model=test_result$model
#' logBF=test_result$logBF
#' y_fitted=test_result$y_fitted
#'
#' @keywords model, Bayes factor
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

apply_gpTest <-
function(x,y,v,nullModelKernelTypes=c("white","fixedvariance"),modelKernelTypes=c("rbf","white","fixedvariance"),y_fitted=FALSE) {

	x=as.matrix(x)
	y=as.matrix(y)
	v=as.matrix(v)
	model=constructModel(x,y,v,nullModelKernelTypes)	
	model=setInitParams(model)	
	model0 = gpOptimise(model, 0)	
	LogLik0=gpLogLikelihood(model0)

	model=constructModel(x,y,v,modelKernelTypes)	
	model=setInitParams(model)	
	model1 = gpOptimise(model, 0)	
	LogLik1=gpLogLikelihood(model1)

	logBF=LogLik1-LogLik0

	result=list("model"=model1,"nullModel"=model0,"logBF"=logBF)

	if (y_fitted==TRUE) {
		y_fitted_model1=as.vector(t(kernCompute(model1$kern, x, x))%*%model1$invK_uu%*%model1$m+(rep(mean(model1$y),dim(x)[1],1)))
		result=c(result,"y_fitted"=y_fitted)
	}

	return(result)

}
