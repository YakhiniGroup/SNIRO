library(data.table)
library(ChemometricsWithR)
library(prospectr)
library(corpcor)
library(fields)
library(BioPhysConnectoR)
library(rJava)
library(doParallel)
library(foreach)
library(gridExtra)

getScatterplots<-function(glucose,S.pred,numls,main,sub,c,bestLs){
par(mar=c(5,4,3,6))  
par(oma=c(0,2,0,0))
#layout(matrix(c(1,0,1,0), 2))
plot(S.pred, as.matrix(glucose), xlab = "Predicted Measured Values", ylab= "Actual Measured Values",main=main,sub=sub,xlim=c(range(S.pred)[1], range(S.pred)[2]), ylim=c(range(as.matrix(glucose))[1], range(as.matrix(glucose))[2]),cex.sub=0.75,cex.axis=0.8)
abline(a=0, b=1, col="red")
tab<-t(matrix(c(bestLs,sapply(c,round,digits=1)),nrow = numls ,ncol = 2))
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.5)),
  colhead = list(fg_params=list(cex = 0.5)),
  rowhead = list(fg_params=list(cex = 0.5)))
pushViewport(viewport(x=0.639,.3,y=0.175,height=.5))
grid.table(tab,rows=c("Wavelength","Constants"),theme=mytheme)
}