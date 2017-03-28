#' @title Constructing GP model with the specified kernels for two samples
#'
#' @description
#' Function for constructing the GP model with the specified kernels
#' (and parameters) for two samples.
#'
#' @param x A list of one-column matrices which contain the input
#' values for each series, i.e., time points for GP models.
#' The values given in this vector are used in GP model, so if any
#' transformation is needed, remember to transform them before
#' constructing the model.
#' @param y A list of one-column matrices which contain the observed
#' values at the corresponding time points given in the \code{x} vector. 
#' @param v A list of one-column matrices which contain the fixed
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
#' x=list(as.matrix(seq(1,10)), as.matrix(seq(1,10)))
#' y=list(as.matrix(sin(x[[1]])), as.matrix(cos(x[[2]])))
#' v=list(as.matrix(runif(10,0,0.2)), as.matrix(runif(10,0,0.2)))
#' kernelTypes=c("rbf","white","fixedvariance")
#' model2=constructTwoSampleModel(x,y,v,kernelTypes)
#' model1=constructModel(rbind(x[[1]], x[[2]]), rbind(y[[1]], y[[2]]), rbind(v[[1]], v[[2]]), kernelTypes)
#'
#' @keywords model fixedvariance
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}; Antti Honkela, \email{antti.honkela@@helsinki.fi}
#' 

constructTwoSampleModel <-
function (x,y,v,kernelTypes,params=NULL) {

    stopifnot(length(x) == length(y))
    stopifnot(length(y) == length(v))
    xmat = matrix(nrow=0, ncol=dim(x[[1]])[2]+1)
    ymat = matrix(nrow=0, ncol=dim(y[[1]])[2])
    vmat = matrix(nrow=0, ncol=dim(v[[1]])[2])
    for (i in seq(length(x))) {
	xmat = rbind(xmat, cbind(i, as.matrix(x[[i]])))
	ymat = rbind(ymat, as.matrix(y[[i]]))
	vmat = rbind(vmat, as.matrix(v[[i]]))
    }
	
	list_kernelTypes=list()

	if ("rbf" %in% kernelTypes) {
		l_bound=min(diff(sort(unique(xmat[,2])))) # set length scale lowerbound to the minimum distance between consecutive time points
		iw_upperbound=1/(l_bound^2)
		iw_lowerbound=0 # l=inf 
		list_rbf=list(type="rbf",options=list(inverseWidthBounds=c(iw_lowerbound,iw_upperbound)))
                for (i in seq(length(x))) {
                    list_selproj=list(type="selproj", comp=list(list_rbf), options=list(expmask=i))
                    list_kernelTypes=append(list_kernelTypes,list(list_selproj))
                }
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
		list_fixedvar=list(type="parametric", realType="fixedvariance",options=list(variance=vmat,input=xmat))
		list_kernelTypes=append(list_kernelTypes,list(list_fixedvar))
	}
	
	options=gpOptions(approx="ftc")
	options$kern=list(type="cmpnd",comp=list_kernelTypes)
	model=gpCreate(dim(xmat)[2], dim(ymat)[2], xmat, ymat, options)

	if (!is.null(params)) {
		model=modelExpandParam(model,params)	
	}

	return(model)

}
