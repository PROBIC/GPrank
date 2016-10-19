#' @title Fitting GP models for the BitSeq example
#'
#' @description
#' Function for fitting two GP models and computing Bayes factors, i.e., the ratio of the maximum marginal
#' likelihood estimates of the two GP models, where the models are:
#' \itemize{
#' \item null model: GP with only white noise covariance matrix
#' \item alternative model: GP with squared exponential and white noise covariance matrices
#' }
#' Optionally, log Bayes factors and the parameter estimates can be written to output files whose names
#' are specified with fileName_logBF,
#' fileName_ModelParams, and fileName_NullModelParams. 
#'
#' @param gpData for example, output from \code{\link{bitseq_rnaSeqData}}.
#' @param fileName_logBF name of the file which contains log Bayes factors.
#' @param fileName_ModelParams name of the file which contains model parameters.
#' @param fileName_NullModelParams name of the file which contains null model parameters.
#'
#' @export
#' @return List of results
#'
#' @examples
#' RNAseqDATA
#' gpData=RNAseqDATA$reltr
#' GPfits=bitseq_fitGPs(gpData)
#' 
#' @keywords GP
#' @seealso \code{\link{bitseq_rnaSeqData}}
#' @author Hande Topa, \email{hande.topa@@helsinki.fi} 

bitseq_fitGPs <-
function(gpData, fileName_logBF=NULL, fileName_ModelParams=NULL, fileName_NullModelParams=NULL) {

	#library("gptk")
	M=gpData$mean
	V=(gpData$std)^2
	N=nrow(M)
	ItemNames=rownames(M)
	x=as.matrix(gpData$time_mapping$time[which(gpData$time_mapping$files %in% colnames(M))])

	nullModelKernelTypes=c("white","fixedvariance")
	ModelKernelTypes=c("rbf","white","fixedvariance")

	nParams_nullModel=1
	nParams_Model=3
	logBayesFactors=matrix(NA,N,1)
	rownames(logBayesFactors)=c(ItemNames)

	Model=list(logLik=matrix(NA,N,1),params=matrix(NA,N,nParams_Model),transformed_params=matrix(NA,N,nParams_Model)) # log-likelihood and parameter estimates for time-dependent model
  Model=lapply(Model, function(x) {
	rownames(x)=c(ItemNames)
	return(x)
	})
	nullModel=list(logLik=matrix(NA,N,1),params=matrix(NA,N,nParams_nullModel),transformed_params=matrix(NA,N,nParams_nullModel)) # log-likelihood and parameter estimates for time-independent model
  nullModel=lapply(nullModel, function(x) {
	rownames(x)=c(ItemNames)
	return(x)
	})

	Model$kernelType=ModelKernelTypes
	nullModel$kernelType=nullModelKernelTypes

	for (i in 1:N) {

		y=as.matrix(M[i,])
		v=as.matrix(V[i,])

		# Set naive version if fixed variances are all zero:
		if (all(v==0)) {
			nullModelKernelTypes=c("white")
			ModelKernelTypes=c("rbf","white")	 
		}

		if (((range(y)[2]-range(y)[1])!=0) & all((is.na(y))!=1)) {
			rslt=gpTest(x,y,v,nullModelKernelTypes,ModelKernelTypes)
			logBayesFactors[i]=rslt$logBF

			nullModel$logLik[i,]=c(gpLogLikelihood(rslt$nullModel))
			nullModel$transformed_params[i,]=c(getModelKernParams(rslt$nullModel,transformed=TRUE))
			nullModel$params[i,]=c(getModelKernParams(rslt$nullModel))
			colnames(nullModel$transformed_params)=names(getModelKernParams(rslt$nullModel,transformed=TRUE))
			colnames(nullModel$params)=names(getModelKernParams(rslt$nullModel))

			Model$logLik[i,]=c(gpLogLikelihood(rslt$model))
			Model$transformed_params[i,]=c(getModelKernParams(rslt$model,transformed=TRUE))		
			Model$params[i,]=c(getModelKernParams(rslt$model))		
			colnames(Model$transformed_params)=names(getModelKernParams(rslt$model,transformed=TRUE))		
			colnames(Model$params)=names(getModelKernParams(rslt$model))
		}

	}
			
	if (!is.null(fileName_logBF)) {
		write.table(logBayesFactors,file=fileName_logBF,quote=F,sep="\t",col.names=NA)
	}
	if (!is.null(fileName_ModelParams)) {
		write.table(cbind(Model$logLik, Model$params),file=fileName_ModelParams,quote=F,sep="\t",col.names=NA)
	}
	if (!is.null(fileName_NullModelParams)) {
		write.table(cbind(nullModel$logLik, nullModel$params),file=fileName_NullModelParams,quote=F,sep="\t",col.names=NA)
	}

	return(list(logBayesFactors=logBayesFactors, Model=Model, nullModel=nullModel))

}
