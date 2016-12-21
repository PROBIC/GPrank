#' @title Building SQLite database
#'
#' @description
#' Function for building an SQLite database for displaying 
#' GP profiles of the selected items on a browser,
#' enabling to rank them according to their Bayes factors 
#' and other provided parameters.
#' For details of using tigreBrowser, please refer to
#' \url{https://github.com/PROBIC/tigreBrowser} and. 
#' \url{https://github.com/PROBIC/tigreBrowserWriter}.
#'
#' @param dbInfo List which contains the required information
#' of the items that will be included in the database.
#' Three arguments must be specified in the \code{dbInfo}:
#' \itemize{
#' \item \code{database_name}: Name of the database.
#' \item \code{database_params}: List of parameters which will be included in the database.
#' \item \code{identifiers}: Identifiers for the items which will be displayed on the browser.
#' These identifiers should appear in the begining of the corresponding figure names, followed by 
#' an underscore and the type of the plot.
#' For example, for identifier "geneA", multiple figures may be named as 
#' "geneA_gene.png", "geneA_abstr.png", and "geneA_reltr.png".
#' }
#' @param figs Character vector containing the figure names. 
#'
#' @export
#' @return Generates an SQLite database named "$database_name.sqlite".
#'
#' @examples
#' BF=c(3,10,2)
#' FoldChange=c(0.5,3,5)
#' dbParams=list("BF"=BF,"Fold change"=FoldChange)
#' identifiers=c("geneA","geneB","geneC")
#' dbInfo=list(database_name="testdb","database_params"=dbParams,"identifiers"=identifiers)
#' figs=c("geneA_gene.png","geneA_abstr.png","geneA_reltr.png","geneB_gene.png",
#' "geneB_abstr.png","geneB_reltr.png","geneC_gene.png","geneC_abstr.png","geneC_reltr.png")
#' for (i in seq(1,9)) {
#' 	examplefig=figs[i] 
#'      png(examplefig)
#'      plot(c(0, 1), c(0, 1))
#'      dev.off()
#' }
#' createDatabase(dbInfo,figs)
#'
#' @import tigreBrowserWriter
#' 
#' @keywords database
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

createDatabase <-
function(dbInfo,figs) {

	project_name=dbInfo$database_name
	db_name=paste(project_name,".sqlite",sep="")
	dbParams=dbInfo$database_params

	db = initializeDb(db_name, project_name)

	aliases=dbInfo$identifiers
	if (is.null(names(aliases))) {
		names(aliases)=aliases
	}
	db = insertAliases(db, project_name, aliases)

	# Insert results:
	l_params=length(dbParams)
	for (i in 1:l_params) {
		param_id=names(dbParams)[i]
		param=dbParams[[i]]
		names(param)=names(aliases)
		db = insertResults(db, param_id, "_", "", param)
	}

	# Insert figures:
	if (length(figs)<1) {
		stop(sprintf("No figures are specified..."))		
	} else {
		fig_extensions=unique(sapply(strsplit(figs,split="\\_"), tail, n = 1))
		n_fig=length(fig_extensions)
		for (f in 1:n_fig) {
			fig_f=figs[grep(figs,pattern=fig_extensions[f])]
			fig_identifiers=sapply(strsplit(fig_f,split="\\_"), head, n = 1)
			fig_identifiers=sapply(strsplit(fig_identifiers,split="/"), tail, n = 1)
			names(fig_f)=fig_identifiers
			fig_f=as.list(fig_f)
			db = insertFigureData(db, param_id, "_", fig_f,name=sapply(strsplit(fig_extensions[[f]],split="\\."), head, n = 1))	
		}
	}

	db = closeDb(db)
	Sys.chmod(db_name, mode = "444")
}
