#' @title Initializing kernel parameters
#'
#' @description
#' Function for setting initial parameters to reasonable values
#' after performing a grid search over parameters within their  
#' pre-set ranges. Loglikelihood is computed for each combination 
#' of parameter values on the grid, and those which lead to the 
#' highest loglikelihood value are set as initial values. 
#' 
#' @param model GP model.
#' @param grid_size Size of the grid over which the search for maximum likelihood will be done. Default value is 5.
#'
#' @export
#' @return Return GP model with the parameters initialized at reasonable values.
#'
#' @examples
#' x=as.matrix(seq(1,10))
#' y=as.matrix(sin(x))
#' v=as.matrix(runif(10,0,0.5))
#' kernelTypes=c("rbf","white","fixedvariance")
#' model=constructModel(x,y,v,kernelTypes)
#' model=setInitParams(model)
#'
#' @keywords initial parameter
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

setInitParams <-
function (model,grid_size=5) {

	no_of_params=model$kern$nParams
	no_of_kernels=length(model$kern$comp)
	paramNames=list()
	for (i in 1:no_of_kernels) {
		paramNames=append(paramNames,model$kern$comp[[i]]$paramNames)
	}

	paramLims=list()

	if ("inverseWidth" %in% paramNames) {
		iw_ind=which(paramNames=="inverseWidth")		
		iw_bounds=model$kern$comp[[iw_ind]]$options$inverseWidthBounds

		l_bound=sqrt(1/iw_bounds[2])
		l_lims=seq(l_bound,c(tail(model$X,1)),length=grid_size)
		l_lims[1]=head(l_lims,1)+1e-14
		l_lims[length(l_lims)]=tail(l_lims,1)-1e-14
		iw_lims=1/(l_lims^2)
		paramLims[[iw_ind]]=boundedTransform(iw_lims, 'xtoa', iw_bounds)
	}

	var_ind=which(paramNames=="variance")
	for (i in 1:length(var_ind)) {
		paramLims[[var_ind[i]]]=seq(-10,c(log(var(model$y))),length=grid_size) # Corresponding variance range is (exp(-10),var(y))
	}

	gridPoints=expand.grid(paramLims)
	
	lenGrid=nrow(gridPoints)

	if (no_of_params!=ncol(gridPoints)) {
		print("Not all of the parameters are included in the grid search.")
		initial_params=c()
	} else {
		LogLik=matrix(0,lenGrid,1)
		for (i in 1:lenGrid) {
			model1 = gpExpandParam(model, as.vector(as.matrix(gridPoints[i,])))
			LogLik[i] = gpLogLikelihood(model1)
		}	
		ind_maxLogLik=which.max(LogLik)
		initial_params=as.vector(as.matrix(gridPoints[ind_maxLogLik,]))
	}

	model=modelExpandParam(model,initial_params) # initial_params are in their transformed form
	return(model)

}


