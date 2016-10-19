#' @title Obtaining counts data in the format of example snpData by using
#' the sample data file. 
#'
#' @description
#' Function for extracting the required data for the experimental evolution application. 
#'
#' @param dataFileName Name of the raw data file
#' @param noHeaderLines Number of header lines, set to 1 by default.
#' @param noInfoColumns Number of columns which contain information about the SNP locations.
#' These columns are used to construct SNP IDs.
#' @param noOptions Number of possible alterations separated by the character defined in "sep"
#' Default is 6, assuming the form A:T:C:G:N:Del
#' @param sep Character which separates the alterations. Default is set to ":".
#'
#' @export
#' @return List of snpData which contains counts in the "counts" matrix and sequencing
#' depth values in the "seq_depth" matrix. SNP IDs are stored as row names, and time
#' points are stored as column names. 
#'
#' @examples
#' # dataFileName="sampleCountsData"
#' # snpData=bbgp_snpData(dataFileName)
#' 
#' @keywords bbgp snpData
#' 
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

bbgp_snpData <-
function(dataFileName,noHeaderLines=1,noInfoColumns=3,noOptions=6,sep=":") {

	headerLines=read.table(dataFileName,skip=0,nrows=noHeaderLines)
	X=as.matrix(as.numeric(headerLines[1,][,-seq(1,noInfoColumns)]))
 	
	sample_data=read.table(dataFileName,skip=noHeaderLines)
	num_of_items=nrow(sample_data)

	J=ncol(sample_data)-noInfoColumns # number of (time,rep) combinations in the data file.

	ID=as.matrix(paste(as.character(sample_data[,1]),"_",as.character(sample_data[,2]),sep=""))
	COUNTS=as.matrix(sample_data[,-seq(1,noInfoColumns)])

	counts=matrix(nrow=num_of_items,ncol=J)
	seq_depth=matrix(nrow=num_of_items,ncol=J)

	for (i in 1:num_of_items) {
		d=COUNTS[i,]
		countsMatrix=matrix(nrow=noOptions,ncol=J)
		for (j in 1:J) {
			d1=d[j]
			s=as.matrix(as.numeric(unlist(strsplit(d1,sep))))
			countsMatrix[,j]=s
		}

		ind_nonzero=unique(which(countsMatrix!=0,arr.ind=TRUE)[,1])
		if (length(ind_nonzero)>2) {
			print(sprintf("SNP on line %d is not bi-allelic. Check the data file.", i))
		} else {
			allele_counts=countsMatrix[ind_nonzero, ,drop=FALSE]
			counts[i,]=matrix(allele_counts[1,],1,J)
			seq_depth[i,]=matrix(colSums(allele_counts),1,J)
		}
	} 
		
	rownames(seq_depth)=ID
	colnames(seq_depth)=X
	rownames(counts)=ID
	colnames(counts)=X
	snpData=list("counts"=counts,"seq_depth"=seq_depth)
	return(snpData)

}

