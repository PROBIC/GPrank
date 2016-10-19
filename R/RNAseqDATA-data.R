#' Sample data obtained from example BitSeq output files
#'
#' Data from an RNA-seq time series experiment.
#' This data set contains mean and standard deviation information on the expression
#' levels of 12 transcripts (which were originated from 4 genes) at 10 time points
#' (0, 5, 10, 20, 40, 80, 160, 320, 640, 1280 mins) for three settings: 'gene',
#' 'abstr' (absolute transcript), and 'reltr' (relative transcript) expression
#' levels. In addition, the fields 'gene_mapping' and 'time_mapping' includes
#' information which is useful to match the genes with transcripts and the time
#' points with data files, respectively.
#'
#' @docType data
#'
#' @usage data(RNAseqDATA)
#'
#' @keywords datasets
#'
#' @examples
#' data(RNAseqDATA)
#' gpData=RNAseqDATA$gene
#' gpData=RNAseqDATA$abstr
#' gpData=RNAseqDATA$reltr
"RNAseqDATA"
