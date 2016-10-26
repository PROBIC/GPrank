#' @title Building SQLite database
#'
#' @description
#' Function for building an SQLite database for displaying 
#' GP profiles of the selected items on a browser,
#' enabling to rank them according to their Bayes factors 
#' and other provided parameters.
#' Please place all figures in a subdirectory and also remember
#' to place tigreBrowser configuration files in the same folder
#' with the database as well.
#' Also modify the database name accordingly
#' in "tigreBrowser.cfg".
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
#' These identifiers should appear in the begining of the corresponding figure names.
#' If multiple figures will be displayed for each item, they should be separated with
#' specific names added after identifier name followed by underscore. 
#' For example, for identifier "geneA", multiple figures may be named as 
#' "geneA_gene.png", "geneA_abstr.png", and "geneA_reltr.png".
#' If there is only a single figure to display, please name it with the corresponding identifier
#' name. For example, for identifier "geneA", name the figure "geneA.png".
#' }
#' @param figuresPath Directory in which the figures are placed in png format.
#' @param multi Logical value indicating whether items have multiple (1) or single (0) figure(s).
#' Default value is set to 0.
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
#' figuresPath="figures/"
#' multi=1
#' createDatabase(dbInfo,figuresPath,multi)
#'
#' @import tigreBrowserWriter
#' 
#' @keywords database
#' @author Hande Topa, \email{hande.topa@@helsinki.fi}
#' 

createDatabase <-
function(dbInfo,figuresPath,multi=0) {

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
	if (multi==0) {
		fig_link=paste(figuresPath,"${probe_name}.png",sep="")
		db = insertFigures(db, param_id, "_", fig_link)
	} else {
		figs <- list.files(path=figuresPath, pattern = "\\.png$")
		fig_extensions=unique(sapply(strsplit(figs,split="\\_"), tail, n = 1))
		n_fig=length(fig_extensions)
		for (f in 1:n_fig) {
			fig_link=paste(figuresPath,"${probe_name}_",fig_extensions[f],sep="")
			db = insertFigures(db, param_id, "_", fig_link)	
		}
	}

	db = closeDb(db)

}
