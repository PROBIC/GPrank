#' Sample data obtained from example BitSeq output files
#'
#' The original data was introduced in (Honkela et al., 2015) and can be accessed
#' in Gene Expression Omnibus (GEO) database (\url{www.ncbi.nlm.nih.gov/geo}) 
#' with accession no. GSE62789.
#' 
#' This data set contains mean and standard deviation information on the expression
#' levels of 5 transcripts (which were originated from 2 genes) at 10 time points
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
#' 
#' @references
#' Antti Honkela, Jaakko Peltonen, Hande Topa, Iryna Charapitsa, Filomena Matarese, Korbinian Grote, Hendrik G. Stunnenberg, George Reid, Neil D. Lawrence, Magnus Rattray.
#' Genome-wide modeling of transcription kinetics reveals patterns of RNA production delays.
#' \emph{PNAS} 112(42):13115-13120, \bold{2015}.
"RNAseqDATA"
