#' @title Computing means and standard deviations for the BBGP (beta binomial Gaussian process) model
#'
#' @description
#' Function for obtaining the posterior means and standard deviations
#' for the frequencies (counts divided by sequencing depth)
#' by using the counts and sequencing depth values in a
#' beta binomial model. Parameters (alpha and beta) of the
#' model are set to 1 by default, which keeps symmetry between
#' f and (1-f), where f denotes the frequency valued between
#' (0,1).
#' 
#' @param x Time vector
#' @param counts Vector containing the counts data at the given time points.
#' @param seq_depth Vector containing the sequencing depth values at the given time points.
#' @param alpha alpha parameter of the beta binomial model.
#' @param beta beta parameter of the beta binomial model.
#' 
#' @export
#' @return Return list containing the posterior means and standard deviations of the frequencies
#' at the time points where sequencing depth is larger than zero. x vector is updated so
#' that it excludes the time points with zero sequencing depth, i.e. time points at which
#' no data have been observed. Posterior means and standard deviations and the updated x vecor
#' are assigned to the list elements named 'posteriorMean', 'posteriorStd', and 'time', respectively.
#'
#' @examples
#' x=c(1,2,3,4,5)
#' counts=c(12,54,32,0,34)
#' seq_depth=c(50,70,35,0,40)
#' bbgp=get_bbgpMeanStd(x,counts,seq_depth)
#' x=bbgp$time # updated time vector
#' y=bbgp$posteriorMean # posterior means
#' v=bbgp$posteriorStd^2 # posterior variances
#'
#' @keywords bbgp
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#'
get_bbgpMeanStd <-
function(x, counts, seq_depth, alpha=1, beta=1) {

	x=as.matrix(x)
	counts=as.matrix(counts)
	seq_depth=as.matrix(seq_depth)
	# Remove the time points at which the SNP has zero coverage, i.e. zero sequencing depth:
	ind_nonzero=which(seq_depth!=0)
	counts=as.matrix(counts[ind_nonzero])
	seq_depth=as.matrix(seq_depth[ind_nonzero])
	x=as.matrix(x[ind_nonzero])

	if (is.unsorted(x)==TRUE) {	
		order_ind=order(x)
		counts=as.matrix(counts[order_ind])
		seq_depth=as.matrix(seq_depth[order_ind])
		x=as.matrix(x[order_ind])
	}

 	y=as.matrix(alpha+counts)/(alpha+beta+seq_depth)
	v=as.matrix((alpha+counts)*(1+seq_depth-counts))/((alpha+beta+seq_depth)^2*(alpha+beta+seq_depth+1))

	bbgp=list("posteriorMean"=y, "posteriorStd"=sqrt(v), "time"=x)

	return(bbgp)

}
