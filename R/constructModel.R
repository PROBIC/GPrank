#' @title Constructing GP model with the specified kernels
#'
#' @description
#' Function for constructing the GP model with the specified kernels
#' (and parameters).
#'
#' @param x One-column matrix which contains the input
#' values, i.e., time points for GP models.
#' The values given in this vector are used in GP model, so if any
#' transformation is needed, remember to transform them before
#' constructing the model.
#' @param y One-column matrix which contains the observed
#' values at the corresponding time points given in the \code{x} vector. 
#' @param v One-column matrix which contains the fixed
#' variances at the corresponding time points given in the \code{x} vector,
#' with the corresponding observations given in the \code{y} vector. 
#' @param kernelTypes Character vector which contains the types of the
#' kernels which will be used in the GP models.
#' Kernel types: \code{'rbf'}, \code{'white'}, \code{'bias'}, \code{'fixedvariance'}.
#' Note that the lower bound for the length scale parameter of rbf kernel
#' is set to the minimum distance between consecutive time points in order
#' to mitigate potential overfitting problems.
#' @param params Values of the kernel parameters in their transformed form.
#' If not specified, default values are assigned.
#'
#' @export
#' @return Return GP model constucted with the specified kernel settings.
#'
#' @examples
#' x=as.matrix(seq(1,10))
#' y=as.matrix(sin(x))
#' v=as.matrix(runif(10,0,0.5))
#' kernelTypes=c("rbf","white","fixedvariance")
#' model=constructModel(x,y,v,kernelTypes)
#'
#' @keywords model fixedvariance
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

constructModel <-
function (x,y,v,kernelTypes,params=NULL) {

	x=as.matrix(x)
	y=as.matrix(y)
	v=as.matrix(v)
	
	list_kernelTypes=list()

	if ("rbf" %in% kernelTypes) {
		l_bound=min(diff(sort(unique(x)))) # set length scale lowerbound to the minimum distance between consecutive time points
		iw_upperbound=1/(l_bound^2)
		iw_lowerbound=0 # l=inf 
		list_rbf=list(type="rbf",options=list(inverseWidthBounds=c(iw_lowerbound,iw_upperbound)))
		list_kernelTypes=append(list_kernelTypes,list(list_rbf))
	}
	if ("bias" %in% kernelTypes) {
		list_bias=list(type="bias")
		list_kernelTypes=append(list_kernelTypes,list(list_bias))
	}
	if ("white" %in% kernelTypes) {
		list_white=list(type="white")
		list_kernelTypes=append(list_kernelTypes,list(list_white))
	}
	if ("fixedvariance" %in% kernelTypes) {
		list_fixedvar=list(type="parametric", realType="fixedvariance",options=list(variance=v,input=x))
		list_kernelTypes=append(list_kernelTypes,list(list_fixedvar))
	}
	
	options=gpOptions(approx="ftc")
	options$kern=list(type="cmpnd",comp=list_kernelTypes)
	model=gpCreate(dim(x)[2], dim(y)[2], x, y, options)

	if (!is.null(params)) {
		model=modelExpandParam(model,params)	
	}

	return(model)

}
