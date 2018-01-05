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

start.time <- Sys.time()
script.dir<- getwd()
dir.create(paste(script.dir,"/","Results",sep = ""))

source(paste(script.dir,"/Functions/","NormalDistofRandom5L.R",sep = ""))
source(paste(script.dir,"/Functions/","getScatterplotsrand.R",sep = ""))
source(paste(script.dir,"/Functions/","result20testgroup.R",sep = ""))
source(paste(script.dir,"/Functions/","turntodataset.R",sep = ""))
source(paste(script.dir,"/Functions/","disjointificationConc.R",sep = ""))
source(paste(script.dir,"/Functions/","Exhaustive_5L.R",sep = ""))
source(paste(script.dir,"/Functions/","SPValuetest.R",sep = ""))
source(paste(script.dir,"/Functions/","CalcRandomVarVector.R",sep = ""))


########################
sampType<- "Diesel Octane Number"
sampSet<- "Training"
sampState<-"Concat" ##### Raw Data/ 1st Der/ Concat
zvnum<-0
whattocalc<- "P" ##### P for Pvalue Z for Zvalue
calcPvalue<-TRUE
calcPvaluerand<-TRUE #To calculate the Pvalue for random sample (Yes or no)
calcZvalue<-FALSE # To calculate the Zvalue (Yes or no)
calcZvaluerand<- FALSE# To calculate the Zvalue for random sample (Yes or no)
rand<-TRUE
rawcor<-0.95
der1cor<-0.8
concatcor<-0.8
numLs<-100
########################
imprtdata <- read.csv(paste(script.dir,"/Data/","dieselNIRTrain.csv",sep = ""),header= TRUE,row.names=1)
imprttest20 <- read.csv(paste(script.dir,"/Data/","dieselNIRTest.csv",sep = ""),header= TRUE,row.names=1)
imprtsampval <- read.csv(paste(script.dir,"/Data/","DieselOctaneTraining.csv",sep = "") ,header = TRUE, row.names = 1)
imprttest20sampval <- read.csv(paste(script.dir,"/Data/","DieselOctaneTest.csv",sep = ""), header= TRUE, row.names = 1)

if (whattocalc=="P"){
  if (calcPvalue==TRUE){
  Zvalue <- SPValuetest(imprtdata,imprtsampval,sampType)
  normalzValue<-as.matrix(do.call(rbind,Zvalue[1])[,1])
  diff1zValue<-as.matrix(do.call(rbind,Zvalue[2])[,1])
  diff2zValue<-as.matrix(do.call(rbind,Zvalue[3])[,1])
  diff3zValue<-as.matrix(do.call(rbind,Zvalue[4])[,1])
  diff4zValue<-as.matrix(do.call(rbind,Zvalue[5])[,1])
  diff5zValue<-as.matrix(do.call(rbind,Zvalue[6])[,1])
}
if (calcPvalue==FALSE){
  normalzValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValuenorm_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  normalzValue<-as.matrix(normalzValue)
  diff1zValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValue1diff_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff1zValue<-as.matrix(diff1zValue)
  diff2zValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValue2diff_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff2zValue<-as.matrix(diff2zValue)
  diff3zValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValue3diff_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff3zValue<-as.matrix(diff3zValue)
  diff4zValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValue4diff_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff4zValue<-as.matrix(diff4zValue)
  diff5zValue <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/PValue5diff_DRY.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff5zValue<-as.matrix(diff5zValue)
}
}
  
####### Concat ##########
if(sampState=="Concat"){
disjointconcat<- disjointificationConc(imprtdata,imprtsampval,normalzValue,diff1zValue,diff2zValue,diff3zValue,diff4zValue,diff5zValue,concatcor,sampType,numLs)
#disjointconcat<- as.matrix(read.xlsx("/Users/MasterOfDisaster/Desktop/Project Alex/Results/Ulva+16Samples/Concat/concat_disjointification0.8Ulva+16.xlsx", sheetIndex = 1, colIndex = c(2,3),header = TRUE))
#disjointconcat<- as.matrix(read.xlsx("/Users/MasterOfDisaster/Desktop/Project Alex/Results/Wet/Concat/concat_disjointification0.8WET.xlsx", sheetIndex = 1, colIndex = c(2,3),header = TRUE))

result5Lconcat<-Exhaustive_5L(imprtdata,imprtsampval,disjointconcat[,1],disjointconcat[,2],zvnum,"")
S.predconcat5L<-do.call(rbind,result5Lconcat[1])
listerrorconcat5L<-do.call(rbind,result5Lconcat[2])
c<-sapply(result5Lconcat[3],round)
bestLs<-gsub("X","",do.call(rbind,result5Lconcat[2])[1:5])
#pdf(file=paste(script.dir,"/results","/","Conc5LTrain.pdf",sep = ""))
#getScatterplots(as.matrix(imprtsampval),as.matrix(S.predconcat5L),5,paste(sampType, sampSet, sampState, "Ls=5", sep=" "),paste("Spearman Cor=", round(as.numeric(listerrorconcat5L[1,7]),3),"    ", "% Error=",round((as.numeric(listerrorconcat5L[1,6])/nrow(imprtdata))*100,3),sep = ""),c,bestLs)
#dev.off()
results5Lconcattest<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampval,listerrorconcat5L[1,c(1,2,3,4,5)])
c.test<-do.call(rbind,results5Lconcattest[4])
#pdf(file=paste(script.dir,"/results","/","Conc5LTest.pdf",sep = ""))
#getScatterplots(as.matrix(imprttest20sampval),do.call(rbind,results5Lconcattest[1]),5,paste(sampType, "Test", sampState, "Ls=5", sep=" "),paste("Spearman Cor=", round(as.numeric(results5Lconcattest[3]),3),"    ", "% Error=",round((as.numeric(results5Lconcattest[2])/nrow(imprttest20))*100,3),sep = ""),c,bestLs)
#dev.off()

if(rand==TRUE){
  randerror<-randomVAR(imprtdata,imprttest20,rawcor,der1cor,concatcor,sampType,imprttest20sampval,imprtsampval=imprtsampval,calcZvaluerand=calcZvaluerand,sampState=sampState,zvnum=zvnum,whattocalc,calcPvaluerand,numLs,c,bestLs)
  randerrtrain5L<- randerror[1]
  randerrtest5L<- randerror[2]
  normaldist(round((as.numeric(listerrorconcat5L[1,6])/nrow(imprtdata))*100,3),randerrtrain5L,imprtsampval,"training")
  normaldist(round((as.numeric(results5Lconcattest[2])/nrow(imprttest20))*100,3),randerrtest5L,imprttest20sampval,"test")
}

}





