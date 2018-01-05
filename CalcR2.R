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

script.dir<- getwd()
dir.create(paste(script.dir,"/","R2",sep = ""))

imprtdata <- read.csv(paste(script.dir,"/Data/","dieselNIRTrain.csv",sep = ""),header= TRUE,row.names=1)
imprttest20 <- read.csv(paste(script.dir,"/Data/","dieselNIRTest.csv",sep = ""),header= TRUE,row.names=1)
imprtsampval <- read.csv(paste(script.dir,"/Data/","DieselOctaneTraining.csv",sep = "") ,header = TRUE, row.names = 1)
imprttest20sampval <- read.csv(paste(script.dir,"/Data/","DieselOctaneTest.csv",sep = ""), header= TRUE, row.names = 1)

numLs<-5
sampType="Diesel"
sampSet="Training"
sampState="Concat"
source(paste(script.dir,"/Functions/","getScatterplots.R",sep = ""))
source(paste(script.dir,"/Functions/","turntodataset.R",sep = ""))

#Fill in Lambdas here:
Lambdas<-c("X992d1","X1018d1","X1266d1","X1016d2","X1022d2")
######################

nir.diff1<-savitzkyGolay((as.matrix(imprtdata)),1,2,3)
colnames(nir.diff1)<-paste(colnames(nir.diff1),"d1",sep = "")
nir.diff2<-savitzkyGolay((as.matrix(imprtdata)),2,3,5)
colnames(nir.diff2)<-paste(colnames(nir.diff2),"d2",sep = "")
nir.diff3<-savitzkyGolay((as.matrix(imprtdata)),3,4,5)
colnames(nir.diff3)<-paste(colnames(nir.diff3),"d3",sep = "")
nir.diff4<-savitzkyGolay((as.matrix(imprtdata)),4,5,7)
colnames(nir.diff4)<-paste(colnames(nir.diff4),"d4",sep = "")
nir.diff5<-savitzkyGolay((as.matrix(imprtdata)),5,6,7)
colnames(nir.diff5)<-paste(colnames(nir.diff5),"d5",sep = "")
colnames(imprtdata)<-paste(colnames(imprtdata),"nm",sep = "")

nir20.diff1<-savitzkyGolay((as.matrix(imprttest20)),1,2,3)
colnames(nir20.diff1)<-paste(colnames(nir20.diff1),"d1",sep = "")
nir20.diff2<-savitzkyGolay((as.matrix(imprttest20)),2,3,5)
colnames(nir20.diff2)<-paste(colnames(nir20.diff2),"d2",sep = "")
nir20.diff3<-savitzkyGolay((as.matrix(imprttest20)),3,4,5)
colnames(nir20.diff3)<-paste(colnames(nir20.diff3),"d3",sep = "")
nir20.diff4<-savitzkyGolay((as.matrix(imprttest20)),4,5,7)
colnames(nir20.diff4)<-paste(colnames(nir20.diff4),"d4",sep = "")
nir20.diff5<-savitzkyGolay((as.matrix(imprttest20)),5,6,7)
colnames(nir20.diff5)<-paste(colnames(nir20.diff5),"d5",sep = "")
colnames(imprttest20)<-paste(colnames(imprttest20),"nm",sep = "")

###################### Training

bestLs<-gsub("X","",Lambdas)
data<-cbind(imprtdata,nir.diff1,nir.diff2,nir.diff3,nir.diff4,nir.diff5)
colnames(imprtsampval)<-c("VAR")
data<- turntodataset(data,imprtsampval)

NIR.pred<-data$NIR[,Lambdas]
lm<-lm(data$VAR~NIR.pred-1)
C<-as.numeric(lm$coefficients)
S.pred<-as.matrix(lm$fitted.values)
spcor<-cor(S.pred,data$VAR, method = "spearman")
relerror<-sqrt(sum(((S.pred-data$VAR)/data$VAR)^2))
R2<-1 - (sum((data$VAR-S.pred )^2)/sum((data$VAR-mean(data$VAR))^2))

spCoretrain<-round(spcor,3)
errorTrain<-round((relerror/nrow(imprtdata))*100,3)
R2train<-round(R2,3)

pdf(file=paste(script.dir,"/R2","/","Conc5LTrain.pdf",sep = ""))
getScatterplots(as.matrix(imprtsampval),as.matrix(S.pred),5,paste(sampType, sampSet, sampState, "Ls=5", sep=" "),paste("Spearman Cor=", spCoretrain,"    ", "% Error=",errorTrain,"     ", "R2=",R2train,sep = ""),C,bestLs)
dev.off()

################Test

testdata<- cbind(imprttest20,nir20.diff1,nir20.diff2,nir20.diff3,nir20.diff4,nir20.diff5)
colnames(imprttest20sampval)<-c("VAR")
testdata<- turntodataset(testdata,imprttest20sampval)

NIRtest.pred<-testdata$NIR[,Lambdas]
Stest.pred<-NIRtest.pred%*%C
relerrorrate<-sqrt(sum(((Stest.pred-imprttest20sampval)/imprttest20sampval)^2))
spcor<-cor(Stest.pred,imprttest20sampval, method = "spearman")
R2<-1 - (sum((imprttest20sampval-Stest.pred )^2)/sum((imprttest20sampval-mean(as.matrix(imprttest20sampval)))^2))
spCoretest<-round(as.numeric(spcor),3)
errortest<-round((as.numeric(relerrorrate)/nrow(imprttest20))*100,3)
R2test<-round(as.numeric(R2),3)

pdf(file=paste(script.dir,"/R2","/","Conc5LTest.pdf",sep = ""))
getScatterplots(as.matrix(imprttest20sampval),Stest.pred,5,paste(sampType, "Test", sampState, "Ls=5", sep=" "),paste("Spearman Cor=",spCoretest,"    ", "% Error=",errortest,"    ","R2=",R2test,sep = ""),C,bestLs)
dev.off()