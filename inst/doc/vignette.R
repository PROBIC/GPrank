### R code from vignette source 'vignette.Rnw'

###################################################
### code chunk number 1: vignette.Rnw:60-61 (eval = FALSE)
###################################################
## devtools::install_github("PROBIC/GPrank")


###################################################
### code chunk number 2: vignette.Rnw:65-66 (eval = FALSE)
###################################################
## install.packages("GPrank")


###################################################
### code chunk number 3: vignette.Rnw:70-71
###################################################
library("GPrank")


###################################################
### code chunk number 4: vignette.Rnw:83-88
###################################################
t=seq(0,20,5)
y=sin(t)
v=0.01*runif(5)
kernelTypes=c('rbf','white','fixedvariance')
model=constructModel(t,y,v,kernelTypes)


###################################################
### code chunk number 5: vignette.Rnw:95-101
###################################################
test_result=gpTest(t,y,v,
nullModelKernelTypes=c("white","fixedvariance"),
modelKernelTypes=c("rbf","white","fixedvariance"))
null_model=test_result$nullModel
model=test_result$model
logBF=test_result$logBF


###################################################
### code chunk number 6: vignette.Rnw:108-112 (eval = FALSE)
###################################################
## color="lightpink" # color=getColorVector()[1]
## ylimits=getYlimits(y,v) # optional argument, also default
## plotGP(model, color, ylimits)
## title(xlab="t", ylab="y")


###################################################
### code chunk number 7: vignette.Rnw:116-120
###################################################
color="lightpink" # color=getColorVector()[1]
ylimits=getYlimits(y,v)
plotGP(model, color, ylimits)
title(xlab="t", ylab="y")


###################################################
### code chunk number 8: vignette.Rnw:141-150
###################################################
BF=c(3,10,2) # Bayes factors
FoldChange=c(0.5,3,5) # Fold changes
dbParams=list("BF"=BF,"Fold change"=FoldChange)
identifiers=c("geneA","geneB","geneC")
dbInfo=list(database_name="testdb","database_params"=dbParams,
"identifiers"=identifiers)
figuresPath="figures/" # directory where the figures are saved
multi=1 # multiple figures will be displayed for each item
createDatabase(dbInfo,figuresPath,multi)


###################################################
### code chunk number 9: vignette.Rnw:158-160
###################################################
library("GPrank")
data(RNAseqDATA)


###################################################
### code chunk number 10: vignette.Rnw:206-213
###################################################
t=log(c(0,5,10,20,40,80,160,320,640,1280)+5) # One can apply
#transforation on time points
names(t)=c("t0000.rpkm","t0005.rpkm","t0010.rpkm","t0020.rpkm",
"t0040.rpkm","t0080.rpkm","t0160.rpkm","t0320.rpkm","t0640.rpkm",
"t1280.rpkm") # matches with the names of the BitSeq output files
trFileName="example_tr"
bitseq_sampleData=bitseq_rnaSeqData(t,trFileName)


###################################################
### code chunk number 11: vignette.Rnw:218-220
###################################################
gene_gpData=RNAseqDATA$gene
gene_GP_models=bitseq_fitGPs(gene_gpData)


###################################################
### code chunk number 12: vignette.Rnw:224-226 (eval = FALSE)
###################################################
## gene_GP_models=bitseq_fitGPs(gene_gpData, fileName_logBF,
## fileName_ModelParams,fileName_NullModelParams)


###################################################
### code chunk number 13: vignette.Rnw:232-240 (eval = FALSE)
###################################################
## item="ARAP2"
## multi=0 # single GP plot in the figure
## ylimits=NULL
## x_ticks=NULL
## x_label="log(5 + t/min)"
## y_label="Expression level (log-rpkm)"
## bitseq_plotGP(item, gene_GP_models, gene_gpData, multi, ylimits,
## x_ticks, x_label, y_label)


###################################################
### code chunk number 14: vignette.Rnw:244-252
###################################################
item="ARAP2"
multi=0 # single GP plot in the figure
ylimits=NULL
x_ticks=NULL
x_label="log(5 + t/min)"
y_label="Expression level (log-rpkm)"
bitseq_plotGP(item, gene_GP_models, gene_gpData, multi, ylimits,
x_ticks, x_label, y_label)


###################################################
### code chunk number 15: vignette.Rnw:260-262 (eval = FALSE)
###################################################
## bitseq_plotGP(item, gene_GP_models, gene_gpData, multi, ylimits,
## x_ticks, x_label, y_label, plotName="ARAP2_gene.png")


###################################################
### code chunk number 16: vignette.Rnw:266-276 (eval = FALSE)
###################################################
## abstr_gpData=RNAseqDATA$abstr
## abstr_GP_models=bitseq_fitGPs(abstr_gpData)
## item="ARAP2"
## multi=1
## ylimits=NULL
## x_ticks=NULL
## x_label="log(5 + t/min)"
## y_label="Expression level (log-rpkm)"
## bitseq_plotGP(item, abstr_GP_models, abstr_gpData, multi, ylimits,
## x_ticks, x_label, y_label)


###################################################
### code chunk number 17: vignette.Rnw:280-290
###################################################
abstr_gpData=RNAseqDATA$abstr
abstr_GP_models=bitseq_fitGPs(abstr_gpData)
item="ARAP2"
multi=1
ylimits=NULL
x_ticks=NULL
x_label="log(5 + t/min)"
y_label="Expression level (log-rpkm)"
bitseq_plotGP(item, abstr_GP_models, abstr_gpData, multi, ylimits,
x_ticks, x_label, y_label)


###################################################
### code chunk number 18: vignette.Rnw:298-309 (eval = FALSE)
###################################################
## reltr_gpData=RNAseqDATA$reltr
## reltr_GP_models=bitseq_fitGPs(reltr_gpData)
## item="ARAP2"
## multi=1
## ylimits=c(0,1) # ratio range between 0 and 1
## x_ticks=NULL
## x_label="log(5 + t/min)"
## y_label="Relative expression level"
## plotName="ARAP2_reltr.pdf"
## bitseq_plotGP(item, reltr_GP_models, reltr_gpData, multi, ylimits,
## x_ticks, x_label, y_label)


###################################################
### code chunk number 19: vignette.Rnw:313-324
###################################################
reltr_gpData=RNAseqDATA$reltr
reltr_GP_models=bitseq_fitGPs(reltr_gpData)
item="ARAP2"
multi=1
ylimits=c(0,1) # ratio range between 0 and 1
x_ticks=NULL
x_label="log(5 + t/min)"
y_label="Relative expression level"
plotName="ARAP2_reltr.pdf"
bitseq_plotGP(item, reltr_GP_models, reltr_gpData, multi, ylimits,
x_ticks, x_label, y_label)


###################################################
### code chunk number 20: vignette.Rnw:343-344
###################################################
data(snpData)


###################################################
### code chunk number 21: vignette.Rnw:348-350
###################################################
dataFileName="sampleCountsData"
sampleSNPdata=bbgp_snpData(dataFileName)


###################################################
### code chunk number 22: vignette.Rnw:355-363
###################################################
x=as.matrix(as.numeric(colnames(snpData$counts)))
# take the fifth SNP in the sample data as example:
counts=as.matrix(snpData$counts[5,]) 
seq_depth=as.matrix(snpData$seq_depth[5,])
bbgp=get_bbgpMeanStd(x,counts,seq_depth)
t=bbgp$time
y=bbgp$posteriorMean
v=(bbgp$posteriorStd)^2


###################################################
### code chunk number 23: vignette.Rnw:367-368
###################################################
snp_gpTest=gpTest(t,y,v)


###################################################
### code chunk number 24: vignette.Rnw:373-377 (eval = FALSE)
###################################################
## model=snp_gpTest$model
## ylims=c(0,1)
## plotGP(model, ylimits=ylims)
## title(xlab="Time", ylab="SNP frequency")


###################################################
### code chunk number 25: vignette.Rnw:381-385
###################################################
model=snp_gpTest$model
ylims=c(0,1)
plotGP(model, ylimits=ylims)
title(xlab="Time", ylab="Frequency")


###################################################
### code chunk number 26: vignette.Rnw:393-394
###################################################
sessionInfo()


