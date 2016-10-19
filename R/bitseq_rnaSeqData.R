#' @title Obtaining data in the format of example RNAseqDATA by using BitSeq output files 
#'
#' @description
#' Function for obtaining the means and standard deviations at the available time points.
#'
#' @param t Vector which contains the input values, i.e., time points. The file names 
#' for the corresponding time point is specified as the names of this vector.
#' @param trFileName Name of the transcriptome file.
#'
#' @export
#' @return List of GP data within the right structure. 
#'
#' @examples
#' # t=log(c(0,5,10,20,40,80,160,320,640,1280)+5)
#' # names(t)=c("t0000.rpkm","t0005.rpkm","t0010.rpkm","t0020.rpkm","t0040.rpkm",
#' # "t0080.rpkm","t0160.rpkm","t0320.rpkm","t0640.rpkm","t1280.rpkm")
#' # trFileName="example_tr"
#' # bitseq_rnaSeqData(t,trFileName)
#' 
#' @keywords GP mean standard deviation
#' 
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

bitseq_rnaSeqData <-
function(t,trFileName) {

	# Get time mapping:

	tu=unique(t)
	nu=as.matrix(as.vector(table(t))) 
	time_mapping=data.frame(files=names(t),time=unname(t),repMask=match(t,tu)) 

	# Get gene mapping:
	
	tr_data=read.table(trFileName, header=TRUE)
	gene_mapping=data.frame(gene_ID=as.matrix(tr_data$gene_ID),transcript_ID=as.matrix(tr_data$transcript_ID),transcript_length=as.matrix(tr_data$transcript_length)) 
	M=length(gene_mapping$transcript_ID)
	nT=length(t) # number of time points / files if replicated

	# Compute expression level means and standard deviations:

	unique_genes_ID=unique(as.matrix(gene_mapping$gene_ID))
	N=length(unique_genes_ID) # number of genes 

	gene=list(logTrans=1,mean=matrix(0,N,nT),std=matrix(0,N,nT),time_mapping=time_mapping,gene_mapping=gene_mapping)
	dimnames(gene$mean) = list(unique_genes_ID,time_mapping$files)
	dimnames(gene$std) = list(unique_genes_ID,time_mapping$files)
	abstr=list(logTrans=1,mean=matrix(0,M,nT),std=matrix(0,M,nT),time_mapping=time_mapping,gene_mapping=gene_mapping)
	dimnames(abstr$mean) = list(gene_mapping$transcript_ID,time_mapping$files)
	dimnames(abstr$std) = list(gene_mapping$transcript_ID,time_mapping$files)
	reltr=list(logTrans=0,mean=matrix(0,M,nT),std=matrix(0,M,nT),time_mapping=time_mapping,gene_mapping=gene_mapping)
	dimnames(reltr$mean) = list(gene_mapping$transcript_ID,time_mapping$files)
	dimnames(reltr$std) = list(gene_mapping$transcript_ID,time_mapping$files)


	gpData=list(gene=gene,abstr=abstr,reltr=reltr)

	for (i in 1:nT)	{

		mcmcFileName=names(t)[i]
		mcmc_data=as.matrix(read.table(mcmcFileName))
		J=ncol(mcmc_data) # number of MCMC samples 
	
		for (k in 1:N) {

			my_gene=unique_genes_ID[k]
			tr_inds=which(gene_mapping$gene_ID %in% my_gene)
			no_tr=length(tr_inds)
			tr_expr=matrix(mcmc_data[tr_inds,],no_tr,J)
			gene_expr=matrix(colSums(tr_expr),1,J)

			gpData$gene$mean[k,i]=rowMeans(log(gene_expr))	
			gpData$gene$std[k,i]=sqrt(rowVars(log(gene_expr)))

			gpData$abstr$mean[tr_inds,i]=rowMeans(log(tr_expr))
			gpData$abstr$std[tr_inds,i]=sqrt(rowVars(log(tr_expr)))
			
			gpData$reltr$mean[tr_inds,i]=rowMeans(tr_expr/(repmat(gene_expr,no_tr,1)))
			gpData$reltr$std[tr_inds,i]=sqrt(rowVars(tr_expr/(repmat(gene_expr,no_tr,1))))
				
		}

	}

	return(gpData)

}

repmat <-
function(x,m,n){

# Function for replicating a matrix with m and n copies of x in the row
# and column dimensions, respectively.
# 
# @param x Matrix which will be replicated.
# @param m Number of rows into which x will be copied.
# @param n Number of columns into which x will be copied.
# 
# @export
# @return Matrix containing m and n copies of x in the row
# and column dimensions, respectively.  

	n_rows = dim(x)[1]
	n_columns = dim(x)[2]
	y=matrix(t(matrix(x,n_rows,n_columns*n)),n_rows*m,n_columns*n,byrow=TRUE)
	return(y)

}
