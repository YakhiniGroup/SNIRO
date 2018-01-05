library(ChemometricsWithR)
library(prospectr)
library(corpcor)
library(fields)
library(BioPhysConnectoR)
getScatterplotsrand<-function(glucose,S.pred,numls,main,sub,Samppredrand,Sampvalrand,c,bestLs){
  max1<-range(c(S.pred,Samppredrand,as.matrix(glucose),as.matrix(Sampvalrand)))[1]
  min1<-range(c(S.pred,Samppredrand,as.matrix(glucose),as.matrix(Sampvalrand)))[2]
  par(mar=c(5,4,3,6))  
  par(oma=c(0,2,0,0))
  plot(S.pred, as.matrix(glucose), xlab = "Predicted Values", ylab= "Actual Measured Values",main=main,sub=sub,xlim=c(max1, min1), ylim=c(max1, min1),cex.sub=0.75,cex.axis=0.8)
  abline(a=0, b=1, col="red")
  points(Samppredrand,as.matrix(Sampvalrand),col="red",pch= 8)
  legend("topleft", legend = c("Actual","Random"), col = c("black", "red"), pch = c(1,8),cex = 0.75)
  tab<-t(matrix(c(bestLs,sapply(c,round,digits=1)),nrow = 5,ncol = 2))
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.5)),
    colhead = list(fg_params=list(cex = 0.5)),
    rowhead = list(fg_params=list(cex = 0.5)))
  pushViewport(viewport(x=0.639,.3,y=0.175,height=.5))
  grid.table(tab,rows=c("Wavelength","Constants"),theme=mytheme)
}