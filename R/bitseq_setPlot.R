#' @title Configuring the settings of the plots for the BitSeq example
#'
#' @description
#' Function for initializing the plotting frame with preset
#' features and with the specified file name. 
#' 
#' @param plotName Name of the plot with either ".png"
#' or ".pdf" extension.
#'
#' @export
#' @return Plotting frame with the specified file name and 
#' preset features.
#'
#' @examples
#' bitseq_setPlot("geneA_GPprofile.pdf")
#' bitseq_setPlot("geneA_GPprofile.png")
#'
#' @keywords plot
#' @keywords internal
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

bitseq_setPlot <-
function(plotName) {

	if (grepl(".pdf",plotName)) {
		pdf(file=plotName)
	} else if (grepl(".png",plotName)) {
		png(filename=plotName)
	} else {
		stop(sprintf("Please specify the file extension as pdf or png in the file name of the plot."))	
	}

}
