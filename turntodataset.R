library(data.table)
library(xlsx)
turntodataset<- function(imprtdata,imprtgluc){
wavelengths <- colnames(imprtdata)
NIR <- imprtdata
NIR <- I(as.matrix(NIR))
VAR<-as.vector(imprtgluc)
rawdata<-data.frame(VAR,NIR)
colnames(rawdata$NIR) = paste(wavelengths)
return(rawdata)
}