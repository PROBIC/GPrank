#' @title Extracting distinctive colors from RColorBrewer package
#'
#' @description
#' Function for obtaining a vector of RGB color codes where the colors are as distinctive as possible from each other. 
#' This function extracts colors from Paired (12 colors), Set2 (8 colors), and Dark2 (8 colors) palettes provided in
#' the RColorBrewer package. If you need more than 28 colors, please remember to add your own palette.
#' 
#' @param opacity Factor modifying the opacity, valued between [0,1]. Default is set to 0.7.
#' @param display By default set to FALSE; if TRUE, displays the color palette.
#'
#' @export
#' @return Return vector of 28 distinctive RGB color codes.
#'
#' @examples
#' color_vector=getColorVector()
#'
#' @import RColorBrewer
#' @importFrom graphics pie
#' @importFrom grDevices adjustcolor
#'
#' @keywords color
#' 
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

getColorVector <-
function(opacity=0.7,display=FALSE) {

	palette_name=c("Paired", "Set2", "Dark2")
	max_no_of_colors=brewer.pal.info$maxcolors[match(palette_name,rownames(brewer.pal.info))] # number of colors in the chosen palettes. One can display the colors in all palettes with display.brewer.all()
	color_vector=unlist(mapply(brewer.pal,max_no_of_colors,palette_name))
	color_vector=c(color_vector[seq(1,max_no_of_colors[1],2)], color_vector[seq(2,max_no_of_colors[1],2)], tail(color_vector,sum(max_no_of_colors)-max_no_of_colors[1]))
	if (display) {
		pie(rep(1,sum(max_no_of_colors)), col=color_vector)
	}
	color_vector=adjustcolor(color_vector, alpha.f=opacity) 

	return(color_vector)

}
