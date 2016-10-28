#' @title Plotting fitted GP models for the BitSeq example
#'
#' @description
#' Function for plotting GP profiles. If the item to be plotted has 
#' multiple items which are associated with itself, its items can
#' be plotted on top of each other by setting \code{multi} to 1,
#' for better visual comparison. Log Bayes factors are displayed 
#' in legends for at most three items which have the largest
#' log Bayes factors. 
#' 
#' @param item Name of the item whose GP profile to be plotted.
#' @param GPfits List structure obtained by \code{bitseq_fitGPs} which contains
#' the time-independent and time-dependent GP models for specified (?) item.
#' @param gpData List structure obtained by, for example, \code{bitseq_rnaSeqData} function.
#' @param multi Indicator value for specifying whether multiple plots (1)
#' or a single plot (0) will be plotted on the same frame. Default value
#' is 0.
#' @param ylimits Numeric vector which contains minimum and maximum limits for the y axis.
#' @param x_ticks X-axis tick labels.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param plotName Name of the plot containing png or pdf extension.
#' png is recommended for the plots which will be displayed on the 
#' browser, whereas pdf is recommended for the plots which will be 
#' used in printed documents. The default plotting settings are
#' specified in \code{bitseq_setPlot} function. If you would like to use 
#' your own preferred settings, remember to specify them on your own.
#' @param colScale RGB color code of the plot. If not specified, colors are 
#' determined by default.
#'
#' @export
#' @return GP plot(s) for the given item.
#'
#' @examples
#' RNAseqDATA
#' gpData=RNAseqDATA$reltr
#' GPfits=bitseq_fitGPs(gpData)
#' item="ARAP2"
#' multi=1
#' ylimits=c(0,1)
#' x_ticks=c("0","5","10","20","40","80","160","320","640","1280")
#' x_label="Time (minutes)"
#' y_label="Expression level (rpkm)"
#' plotName="ARAP2_reltr.pdf"
#' bitseq_plotGP(item, GPfits, gpData, multi, ylimits, x_ticks, x_label, y_label, plotName)
#'
#' @keywords GP plot multi
#' @seealso \code{\link{getColorVector}} \code{\link{bitseq_setPlot}} \code{\link{bitseq_fitGPs}} \code{\link{bitseq_rnaSeqData}}
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 
bitseq_plotGP <-
function(item, GPfits, gpData, multi=0, ylimits=NULL, x_ticks=NULL, x_label=NULL, y_label=NULL, plotName=NULL, colScale=getColorVector()) {

	mapping=gpData$gene_mapping

	if (is.element(item, mapping$gene_ID)==1) {
		if (multi==1) {
			item=mapping$transcript_ID[as.matrix(which(mapping$gene_ID %in% item))]
			item
			colScale=colScale[-1]
		}
	} else {
		if (is.element(item, mapping$transcript_ID)==0) {
			stop(sprintf("No info found for the given item: %s",item))
		}	
	}

	l=length(item)

	if (l>1) {
		message(paste0("Plotting ", as.character(l) ," items..."))
	} else if (l==1) {
		message(paste0("Plotting ", as.character(l) ," item..."))
	} else {
		stop(sprintf("No item found for plotting."))
	}

	x=as.matrix(gpData$time_mapping$time)
	Y=matrix(gpData$mean[which(rownames(gpData$mean) %in% item),],l,length(x))
	V=matrix(gpData$std[which(rownames(gpData$std) %in% item),]^2,l,length(x))
	kernelType=GPfits$Model$kernelType

	logBF=GPfits$logBayesFactors[which(rownames(gpData$mean) %in% item),] 
	indices_for_legend=sort(logBF, decreasing=TRUE, index.return=TRUE)$ix[1:3]
	indices_for_legend=indices_for_legend[!is.na(indices_for_legend)]	

	if (is.null(ylimits)) {
		ylimits=getYlimits(Y,V)
	} 
	
	if (missing(plotName) | is.null(plotName)) {
		frame()
	} else {
		bitseq_setPlot(plotName)
	}
	par(oma = c(1, 1, 6, 1), mar=c(2, 2, 3, 0)+0.6, mgp=c(1.5, 0.5, 0), xpd=TRUE)

	for (i in seq(1:l)) {
		y=as.matrix(Y[i,])
		v=as.matrix(V[i,])
		params=GPfits$Model$transformed_params[which(rownames(gpData$mean) %in% item)[i],]  # transformed parameter values
		col_i=colScale[i]		
		model=constructModel(x,y,v,kernelType,params) 
		suppressWarnings(par(oma = c(0, 0, 0, 0), new = TRUE))
                if (i == 1) {
                  if (is.null(x_ticks)) {
                    plotGP(model,col_i,ylimits, write_xticks=TRUE, write_yticks=TRUE)
                  } else {
                    plotGP(model,col_i,ylimits, write_xticks=FALSE, write_yticks=TRUE)
                  }
                } else {
                  plotGP(model,col_i,ylimits, write_xticks=FALSE, write_yticks=FALSE)
                }
	}

	yRange=seq(min(ylimits),max(ylimits),length=5)	
	if (!is.null(x_ticks)) {
          axis(side = 1, at = c(x), labels=x_ticks)
        }
	title(xlab=x_label, ylab=y_label)
	legend("top", inset=c(0,-0.13), legend=item[indices_for_legend], col=head(colScale,l)[indices_for_legend], lwd=3, xpd=TRUE, horiz = TRUE, bty = "n")	# item names
	legend("top", inset=c(0,-0.08), legend=paste("logBF: ",c(format(logBF[indices_for_legend],digits=3,nsmall=3))), col=head(colScale,l)[indices_for_legend], lwd=3, xpd=TRUE, horiz = TRUE, bty = "n")	# log Bayes factors
	box()
	if (!missing(plotName) | !is.null(plotName)) {
		dev.off()
	}
	
}
