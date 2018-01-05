randomVAR <- function(imprtdata,imprttest20,rawcor,der1cor,concatcor,sampType,imprttest20sampval,imprtsampval,calcZvaluerand,sampState,zvnum,whattocalc,calcPvaluerand,numLs,c,bestLs){
if(whattocalc=="Z"){
if(calcZvaluerand==TRUE){  
  imprtsampvalrand<-as.matrix((sample(as.matrix(imprtsampval))))
  #imprtsampvalrand<-as.matrix(runif(nrow(imprtsampval),range(imprtsampval)[1],range(imprtsampval)[2]))
  colnames(imprtsampvalrand)<-c("VAR")
  write.xlsx(imprtsampvalrand, paste(script.dir,"/Results/","RandomSample.xlsx"))
  ZValuerand <- Zvaluetest(imprtdata,imprtsampvalrand,"Rand")
  normalzValuerand<-do.call(rbind,ZValuerand[1])
  diff1zValuerand<-do.call(rbind,ZValuerand[2])
  diff2zValuerand<-do.call(rbind,ZValuerand[3])
  diff3zValuerand<-do.call(rbind,ZValuerand[4])
  diff4zValuerand<-do.call(rbind,ZValuerand[5])
  diff5zValuerand<-do.call(rbind,ZValuerand[6])
  normalzValuerand<-as.matrix(normalzValuerand)
  diff1zValuerand<-as.matrix(diff1zValuerand)
  diff2zValuerand<-as.matrix(diff2zValuerand)
  diff3zValuerand<-as.matrix(diff3zValuerand)
  diff4zValuerand<-as.matrix(diff4zValuerand)
  diff5zValuerand<-as.matrix(diff5zValuerand)
}
else{
  imprtsampvalrand<-read.xlsx("/Users/MasterOfDisaster/Desktop/RandomSample.xlsx",sheetIndex = 1, colIndex = 2 ,header = TRUE)
  colnames(imprtsampvalrand)<-c("VAR")
  normalzValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuenorm_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  normalzValuerand<-as.matrix(normalzValuerand)
  diff1zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuediff1_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff1zValuerand<-as.matrix(diff1zValuerand)
  diff2zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuediff2_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff2zValuerand<-as.matrix(diff2zValuerand)
  diff3zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuediff3_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff3zValuerand<-as.matrix(diff3zValuerand)
  diff4zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuediff4_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff4zValuerand<-as.matrix(diff4zValuerand)
  diff5zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/ZValuediff5_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
  diff5zValuerand<-as.matrix(diff5zValuerand)
}
}
if(whattocalc=="P"){
  if(calcPvaluerand==TRUE){  
    imprtsampvalrand<-as.matrix((sample(as.matrix(imprtsampval))))
    #imprtsampvalrand<-as.matrix(runif(nrow(imprtsampval),range(imprtsampval)[1],range(imprtsampval)[2]))
    colnames(imprtsampvalrand)<-c("VAR")
    write.csv(imprtsampvalrand, paste(script.dir,"/Results/","RandomSample.csv",sep = ""))
    Zvaluerand <- SPValuetest(imprtdata,imprtsampvalrand,"Rand")
    normalzValuerand<-as.matrix(do.call(rbind,Zvaluerand[1])[,2])
    diff1zValuerand<-as.matrix(do.call(rbind,Zvaluerand[2])[,2])
    diff2zValuerand<-as.matrix(do.call(rbind,Zvaluerand[3])[,2])
    diff3zValuerand<-as.matrix(do.call(rbind,Zvaluerand[4])[,2])
    diff4zValuerand<-as.matrix(do.call(rbind,Zvaluerand[5])[,2])
    diff5zValuerand<-as.matrix(do.call(rbind,Zvaluerand[6])[,2])
  }
  else{
    imprtsampvalrand<-read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/RandomSample.xlsx",sheetIndex = 1, colIndex = 2 ,header = TRUE)
    colnames(imprtsampvalrand)<-c("VAR")
    normalzValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValuenorm_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    normalzValuerand<-as.matrix(normalzValuerand)
    diff1zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValue1diff_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    diff1zValuerand<-as.matrix(diff1zValuerand)
    diff2zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValue2diff_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    diff2zValuerand<-as.matrix(diff2zValuerand)
    diff3zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValue3diff_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    diff3zValuerand<-as.matrix(diff3zValuerand)
    diff4zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValue4diff_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    diff4zValuerand<-as.matrix(diff4zValuerand)
    diff5zValuerand <- read.xlsx("/Users/MasterOfDisaster/Desktop/DRYUlvaConcat/Random/PValue5diff_Rand.xlsx", sheetIndex = 1,header = TRUE,row.names=1)
    diff5zValuerand<-as.matrix(diff5zValuerand)
  }  
}
if(sampState=="Raw Data"){
  L.disjoint=0
  i=0
  while(L.disjoint<numLs){
    disjointrawrand<- disjointificationRaw(imprtdata,imprtsampvalrand,normalzValuerand,cornum=rawcor+i,sampType,numLs)
    i=i+0.01
    L.disjoint<-nrow(disjointrawrand)
  }
  ####### 3 LS
  result3Lrawrand<-Exhaustive_3L(imprtdata,imprtsampvalrand,disjointrawrand[,1],disjointrawrand[,2],zvnum,sampType)
  S.predraw3Lrand<-do.call(rbind,result3Lrawrand[1])
  listerrorraw3Lrand<-do.call(rbind,result3Lrawrand[2])
  results3Lrawtestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerrorraw3Lrand[1,c(1,2,3)])
  
  pdf(file="/Users/MasterOfDisaster/Desktop/RawData3LTrainingRand.pdf")
  getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.predraw3L),3,paste(sampType, sampSet, sampState,"with random","Ls=3", sep="    "),paste("Spearman Cor=", round(as.numeric(listerrorraw3L[1,5]),3)," |","Spearman Cor rand=",round(as.numeric(listerrorraw3Lrand[1,5]),3),"    ", "% Error=",round((as.numeric(listerrorraw3L[1,4])/nrow(imprtdata))*100,3)," |","% Error rand=",round((as.numeric(listerrorraw3Lrand[1,4])/nrow(imprtdata))*100,3),sep = " ", " | PValue=",round(as.numeric(listerrorraw3L[1,6]),3)),S.predraw3Lrand,imprtsampvalrand)
  dev.off()
  
  pdf(file="/Users/MasterOfDisaster/Desktop/RawData3LTestRand.pdf")
  getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results3Lrawtest[1]),3,paste(sampType, "Test", sampState, "Ls=3", sep="    "), paste("Spearman Cor=", round(as.numeric(results3Lrawtest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results3Lrawtestrand[3]),3),"    ", "% Error=",round((as.numeric(results3Lrawtest[2])/nrow(imprttest20))*100,3)," |","% Error rand=",round((as.numeric(results3Lrawtestrand[2])/nrow(imprttest20))*100,3),sep = " ", " |  PValue=", round(as.numeric(results3Lrawtest[4]),3)),do.call(rbind,results3Lrawtestrand[1]),imprttest20sampval)
  dev.off()
  
  ####### 5 LS
  
  result5Lrawrand<-Exhaustive_5L(imprtdata,imprtsampvalrand,disjointrawrand[,1],disjointrawrand[,2],zvnum,sampType)
  S.predraw5Lrand<-do.call(rbind,result5Lrawrand[1])
  listerrorraw5Lrand<-do.call(rbind,result5Lrawrand[2])
  results5Lrawtestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerrorraw5Lrand[1,c(1,2,3,4,5)])
  
  pdf(file="/Users/MasterOfDisaster/Desktop/RawData5LTrainingRand.pdf")
  getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.predraw5L),5,paste(sampType, sampSet, sampState,"with random","Ls=5", sep="    "),paste("Spearman Cor=", round(as.numeric(listerrorraw5L[1,7]),3)," |","Spearman Cor rand=",round(as.numeric(listerrorraw5Lrand[1,7]),3),"    ", "% Error=",round((as.numeric(listerrorraw5L[1,6])/nrow(imprtdata))*100,3)," |","% Error rand=",round((as.numeric(listerrorraw5Lrand[1,6])/nrow(imprtdata))*100,3),sep = " ", " |  PValue=",round(as.numeric(listerrorraw5L[1,8]),3)),S.predraw5Lrand,imprtsampvalrand)
  dev.off()
  
  pdf(file="/Users/MasterOfDisaster/Desktop/RawData5LTestRand.pdf")
  getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results5Lrawtest[1]),5,paste(sampType, "Test", sampState, "Ls=5", sep="    "), paste("Spearman Cor=", round(as.numeric(results5Lrawtest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results5Lrawtestrand[3]),3),"    ", "% Error=",round((as.numeric(results5Lrawtest[2])/nrow(imprttest20))*100,3)," |","% Error rand=",round((as.numeric(results5Lrawtestrand[2])/nrow(imprttest20))*100,3),sep = " ", "  | PValue=", round(as.numeric(results5Lrawtest[4]),3)),do.call(rbind,results5Lrawtestrand[1]),imprttest20sampval)
  dev.off()
  
  return(c(round((as.numeric(listerrorraw3Lrand[1,4])/nrow(imprtdata))*100,3),round((as.numeric(results3Lrawtestrand[2])/nrow(imprttest20))*100,3),round((as.numeric(listerrorraw5Lrand[1,6])/nrow(imprtdata))*100,3),round((as.numeric(results5Lrawtestrand[2])/nrow(imprttest20))*100,3)))
}

if(sampState== "1st Der"){
    disjoint1derrand<- disjointification1stDer(imprtdata,imprtsampvalrand,diff1zValuerand,cornum=der1cor,sampType,numLs)
  ####### 3 LS
  result3L1derrand<-Exhaustive_3L(imprtdata,imprtsampvalrand,disjoint1derrand[,1],disjoint1derrand[,2],0.8,sampType)
  S.pred1der3Lrand<-do.call(rbind,result3L1derrand[1])
  listerror1der3Lrand<-do.call(rbind,result3L1derrand[2])
  results3L1dertestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerror1der3Lrand[1,c(1,2,3)])

  pdf(file="/Users/MasterOfDisaster/Desktop/1der3LTrainingRand.pdf")
  getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.pred1der3L),3,paste(sampType, sampSet, sampState,"with random","Ls=3", sep="    "),paste("Spearman Cor=", round(as.numeric(listerror1der3L[1,5]),3)," |","Spearman Cor rand=",round(as.numeric(listerror1der3Lrand[1,5]),3),"    ", "% Error=",round((as.numeric(listerror1der3L[1,4])/nrow(imprtdata))*100,3)," |","% Error rand=",round((as.numeric(listerror1der3Lrand[1,4])/nrow(imprtdata))*100,3),sep = " ", "  | PValue=",round(as.numeric(listerror1der3L[1,6]),3)),S.pred1der3Lrand,imprtsampvalrand)
  dev.off()
  
  pdf(file="/Users/MasterOfDisaster/Desktop/1der3LTestRand.pdf")
  getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results3L1dertest[1]),3,paste(sampType, "Test", sampState, "Ls=3", sep="    "), paste("Spearman Cor=", round(as.numeric(results3L1dertest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results3L1dertestrand[3]),3),"    ", "% Error=",round((as.numeric(results3L1dertest[2])/nrow(imprttest20))*100,3)," |","% Error rand=",round((as.numeric(results3L1dertestrand[2])/nrow(imprttest20))*100,3),sep = " ", " |  PValue=", round(as.numeric(results3L1dertest[4]),3)),do.call(rbind,results3L1dertestrand[1]),imprttest20sampval) 
  dev.off()
  
  ####### 5 LS
  
  result5L1derrand<-Exhaustive_5L(imprtdata,imprtsampvalrand,disjoint1derrand[,1],disjoint1derrand[,2],0.8,sampType)
  S.pred1der5Lrand<-do.call(rbind,result5L1derrand[1])
  listerror1der5Lrand<-do.call(rbind,result5L1derrand[2])
  results5L1dertestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerror1der5Lrand[1,c(1,2,3,4,5)])
  
  pdf(file="/Users/MasterOfDisaster/Desktop/1der5LTrainingRand.pdf")
  getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.pred1der5L),5,paste(sampType, sampSet, sampState,"with random","Ls=5", sep="    "),paste("Spearman Cor=", round(as.numeric(listerror1der5L[1,7]),3)," |","Spearman Cor rand=",round(as.numeric(listerror1der5Lrand[1,7]),3),"    ", "% Error=",round((as.numeric(listerror1der5L[1,6])/nrow(imprtdata))*100,3)," |","% Error rand=",round((as.numeric(listerror1der5Lrand[1,6])/nrow(imprtdata))*100,3),sep = " ", "  | PValue=",round(as.numeric(listerror1der5L[1,8]),3)),S.pred1der5Lrand,imprtsampvalrand)
  dev.off()
  
  pdf(file="/Users/MasterOfDisaster/Desktop/1der5LTestRand.pdf")
  getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results5L1dertest[1]),5,paste(sampType, "Test", sampState, "Ls=5", sep="    "), paste("Spearman Cor=", round(as.numeric(results5L1dertest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results5L1dertestrand[3]),3),"    ", "% Error=",round((as.numeric(results5L1dertest[2])/nrow(imprttest20))*100,3)," |","% Error rand=",round((as.numeric(results5L1dertestrand[2])/nrow(imprttest20))*100,3),sep = " ", " |  PValue=", round(as.numeric(results5L1dertest[4]),3)),do.call(rbind,results5L1dertestrand[1]),imprttest20sampval)
  dev.off()
  
  return(c(round((as.numeric(listerror1der3Lrand[1,4])/nrow(imprtdata))*100,3),round((as.numeric(results3L1dertestrand[2])/nrow(imprttest20))*100,3),round((as.numeric(listerror1der5Lrand[1,6])/nrow(imprtdata))*100,3),round((as.numeric(results5L1dertestrand[2])/nrow(imprttest20))*100,3)))
  
}
if(sampState== "Concat"){

disjointconcatrand<- disjointificationConc(imprtdata,imprtsampvalrand,normalzValuerand,diff1zValuerand,diff2zValuerand,diff3zValuerand,diff4zValuerand,diff5zValuerand,cornum=concatcor,sampType,numLs)

  #disjointconcatrand<- as.matrix(read.xlsx("/Users/MasterOfDisaster/Desktop/concat_disjointification0.8DRY.xlsx", sheetIndex = 1, colIndex = c(2,3),header = TRUE))
  ####### 3 LS
  #result3Lconcatrand<-Exhaustive_3L(imprtdata,imprtsampvalrand,disjointconcatrand[,1],disjointconcatrand[,2],0,sampType)
  #S.predconcat3Lrand<-do.call(rbind,result3Lconcatrand[1])
  #listerrorconcat3Lrand<-do.call(rbind,result3Lconcatrand[2])
  #results3Lconcattestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerrorconcat3Lrand[1,c(1,2,3)])

  #pdf(file="/Users/MasterOfDisaster/Desktop/Concat3LTrainingRand.pdf")
  #getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.predconcat3L),3,paste(sampType, sampSet, sampState,"with random","Ls=3", sep="    "),paste("Spearman Cor=", round(as.numeric(listerrorconcat3L[1,5]),3)," |","Spearman Cor rand=",round(as.numeric(listerrorconcat3Lrand[1,5]),3),"    ", "% Error=",round((as.numeric(listerrorconcat3L[1,4])/nrow(imprtdata))*100,3)," |","% Error rand=",round((as.numeric(listerrorconcat3Lrand[1,4])/nrow(imprtdata))*100,3),sep = " ", "  | PValue=",round(as.numeric(listerrorconcat3L[1,6]),3)),S.predconcat3Lrand,imprtsampvalrand)
  #dev.off()

  #pdf(file="/Users/MasterOfDisaster/Desktop/Concat3LTestRand.pdf")
  #getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results3Lconcattest[1]),3,paste(sampType, "Test", sampState, "Ls=3", sep="    "), paste("Spearman Cor=", round(as.numeric(results3Lconcattest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results3Lconcattestrand[3]),3),"    ", "% Error=",round((as.numeric(results3Lconcattest[2])/nrow(imprttest20))*100,3)," |","% Error rand=",round((as.numeric(results3Lconcattestrand[2])/nrow(imprttest20))*100,3),sep = " ", " |  PValue=", round(as.numeric(results3Lconcattest[4]),3)),do.call(rbind,results3Lconcattestrand[1]),imprttest20sampval) 
  #dev.off()
  
  ####### 5 LS
  
  result5Lconcatrand<-Exhaustive_5L(imprtdata,imprtsampvalrand,disjointconcatrand[,1],disjointconcatrand[,2],0,"Random")
  S.predconcat5Lrand<-do.call(rbind,result5Lconcatrand[1])
  listerrorconcat5Lrand<-do.call(rbind,result5Lconcatrand[2])
  results5Lconcattestrand<-results20testgroup(imprttest20,imprttest20sampval,imprtdata,imprtsampvalrand,listerrorconcat5Lrand[1,c(1,2,3,4,5)])
  
  pdf(file=paste(script.dir,"/Results/","Concat5LTrainingRand.pdf",sep = ""))
  getScatterplotsrand(as.matrix(imprtsampval),as.matrix(S.predconcat5L),5,paste(sampType, sampSet, sampState,"with random","L=5", sep="    "),paste("Spearman Cor=", round(as.numeric(listerrorconcat5L[1,7]),3)," |","Spearman Cor rand=",round(as.numeric(listerrorconcat5Lrand[1,7]),3),"    ", " TRE=",round((as.numeric(listerrorconcat5L[1,6])/nrow(imprtdata))*100,3)," |","TRE rand=",round((as.numeric(listerrorconcat5Lrand[1,6])/nrow(imprtdata))*100,3),sep = " "),S.predconcat5Lrand,imprtsampvalrand,c,bestLs)
  dev.off()
  
  pdf(file=paste(script.dir,"/Results/","Concat5LTestRand.pdf",sep = ""))
  getScatterplotsrand(as.matrix(imprttest20sampval),do.call(rbind,results5Lconcattest[1]),5,paste(sampType, "Test", sampState, "L=5", sep="    "), paste("Spearman Cor=", round(as.numeric(results5Lconcattest[3]),3)," |","Spearman Cor rand=",round(as.numeric(results5Lconcattestrand[3]),3),"    ", " TRE=",round((as.numeric(results5Lconcattest[2])/nrow(imprttest20))*100,3)," |","TRE rand=",round((as.numeric(results5Lconcattestrand[2])/nrow(imprttest20))*100,3),sep = " "),do.call(rbind,results5Lconcattestrand[1]),imprttest20sampval,c,bestLs)
  dev.off()
  
  return(c(round((as.numeric(listerrorconcat5Lrand[1,6])/nrow(imprtdata))*100,3),round((as.numeric(results5Lconcattestrand[2])/nrow(imprttest20))*100,3)))
  
}
}