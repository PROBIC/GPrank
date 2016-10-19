#' @title Getting the values of the kernel parameters of the GP model
#'
#' @description
#' Function for getting the kernel parameter values of the GP model. In order to get the
#' transformed values, set transformed to TRUE.
#  This function can be used to extract the untransformed parameter values, unlike the
#  modelExtractParam function in gptk package which extracts the transformed parameter
#  values which are then used in the optimization. 
#'
#' @param model GP model 
#' @param transformed Logical variable indicating whether the transformed values of the
#' parameters are desired or not. Default is set to FALSE.
#'
#' @export
#' @return Return vector of values of the kernel parameters of the GP model.
#'
#' @examples
#' x=as.matrix(seq(1,10))
#' y=as.matrix(sin(x))
#' v=as.matrix(runif(10,0,0.5))
#' kernelTypes=c("rbf","white","fixedvariance")
#' model=constructModel(x,y,v,kernelTypes)
#' params=getModelKernParams(model)
#'
#' @keywords parameter
#' @author Hande Topa, \email{hande.topa@@helsinki.fi} 

getModelKernParams <-
function(model,transformed=FALSE) {
	
	params=c()
	no_of_kernels=length(model$kern$comp)
	for (i in seq(1:no_of_kernels)) {
		if (!is.null(model$kern$comp[[i]]$paramNames)) {  # no estimated parameters in fixedvariance kernel
			kern_params=sapply(model$kern$comp[[i]]$paramNames,function(x) getElement(model$kern$comp[[i]],x))
			names(kern_params)=paste(model$kern$comp[[i]]$type,names(kern_params))
			params=c(params,kern_params)
		}
	}

	if (transformed==TRUE) {
		param_names=names(params)
		param_names=paste("transformed",param_names)
		params=gpExtractParam(model)
		names(params)=param_names
	}

	return(params)

}
