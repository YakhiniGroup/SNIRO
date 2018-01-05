
SPValuetest <- function(imprtdata,imprtsampval, sampType){
colnames(imprtsampval)<-c("VAR")
data<-turntodataset(imprtdata,imprtsampval)
var<-as.matrix(data$VAR)
normspec<-data$NIR
nir.diff1<-savitzkyGolay((as.matrix(data$NIR)),1,2,3)
nir.diff2<-savitzkyGolay((as.matrix(data$NIR)),2,3,5)
nir.diff3<-savitzkyGolay((as.matrix(data$NIR)),3,4,5)
nir.diff4<-savitzkyGolay((as.matrix(data$NIR)),4,5,7)
nir.diff5<-savitzkyGolay((as.matrix(data$NIR)),5,6,7)
nir.diff6<-savitzkyGolay((as.matrix(data$NIR)),6,7,9)
count=1
normalPval<-c()
normalRho<-c()
for (i in 1:ncol(normspec)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(normspec[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  normalPval<- c(normalPval,pval)
  normalRho<- c(normalRho, r)
}
normalPval<-t(matrix(normalPval,ncol(normspec)))
colnames(normalPval)<-paste(colnames(normspec),"nm",sep = "")
normalPval<-rbind(normalPval,normalRho)
row.names(normalPval)<-c("Pvalue","Rho")
write.csv(t(normalPval),paste(script.dir,"/Results","/","PValuenorm_",sampType,".csv", sep = ""))
print("finished normal data")
#####################################
count=1
diff1Pval<-c()
diff1Rho<-c()
for (i in 1:ncol(nir.diff1)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(nir.diff1[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  diff1Pval<- c(diff1Pval,pval)
  diff1Rho<- c(diff1Rho, r)
}
diff1Pval<-t(matrix(diff1Pval,ncol(nir.diff1)))
colnames(diff1Pval)<-paste(colnames(nir.diff1),"d1",sep = "")
diff1Pval<-rbind(diff1Pval,diff1Rho)
row.names(diff1Pval)<-c("Pvalue","Rho")
write.csv(t(diff1Pval),paste(script.dir,"/Results","/","PValue1diff_",sampType,".csv", sep = ""))
print("finished 1st Der data")
#####################################
count=1
diff2Pval<-c()
diff2Rho<-c()
for (i in 1:ncol(nir.diff2)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(nir.diff2[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  diff2Pval<- c(diff2Pval,pval)
  diff2Rho<- c(diff2Rho, r)
}
diff2Pval<-t(matrix(diff2Pval,ncol(nir.diff2)))
colnames(diff2Pval)<-paste(colnames(nir.diff2),"d2",sep = "")
diff2Pval<-rbind(diff2Pval,diff2Rho)
row.names(diff2Pval)<-c("Pvalue","Rho")
write.csv(t(diff2Pval),paste(script.dir,"/Results","/","PValue2diff_",sampType,".csv", sep = ""))
print("finished 2nd Der data")
#####################################
count=1
diff3Pval<-c()
diff3Rho<-c()
for (i in 1:ncol(nir.diff3)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(nir.diff3[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  diff3Pval<- c(diff3Pval,pval)
  diff3Rho<- c(diff3Rho, r)
}
diff3Pval<-t(matrix(diff3Pval,ncol(nir.diff3)))
colnames(diff3Pval)<-paste(colnames(nir.diff3),"d3",sep = "")
diff3Pval<-rbind(diff3Pval,diff3Rho)
row.names(diff3Pval)<-c("Pvalue","Rho")
write.csv(t(diff3Pval),paste(script.dir,"/Results","/","PValue3diff_",sampType,".csv", sep = ""))
print("finished 3rd Der data")
#####################################
count=1
diff4Pval<-c()
diff4Rho<-c()
for (i in 1:ncol(nir.diff4)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(nir.diff4[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  diff4Pval<- c(diff4Pval,pval)
  diff4Rho<- c(diff4Rho, r)
}
diff4Pval<-t(matrix(diff4Pval,ncol(nir.diff4)))
colnames(diff4Pval)<-paste(colnames(nir.diff4),"d4",sep = "")
diff4Pval<-rbind(diff4Pval,diff4Rho)
row.names(diff4Pval)<-c("Pvalue","Rho")
write.csv(t(diff4Pval),paste(script.dir,"/Results","/","PValue4diff_",sampType,".csv", sep = ""))
print("finished 4th Der data")
#####################################
count=1
diff5Pval<-c()
diff5Rho<-c()
for (i in 1:ncol(nir.diff5)){
  count<-count+1
  print(count)
  r<-as.numeric(cor.test(nir.diff5[,i],var,method="s")$estimate)
  n<-length(var)
  z<- sqrt((n-3)/1.06)*atanh(r)
  pval<-1-pnorm(abs(z))
  diff5Pval<- c(diff5Pval,pval)
  diff5Rho<- c(diff5Rho, r)
}
diff5Pval<-t(matrix(diff5Pval,ncol(nir.diff5)))
colnames(diff5Pval)<-paste(colnames(nir.diff5),"d5",sep = "")
diff5Pval<-rbind(diff5Pval,diff5Rho)
row.names(diff5Pval)<-c("Pvalue","Rho")
write.csv(t(diff5Pval),paste(script.dir,"/Results","/","PValue5diff_",sampType,".csv", sep = ""))
print("finished 5th Der data")

return(list(t(normalPval),t(diff1Pval),t(diff2Pval),t(diff3Pval),t(diff4Pval),t(diff5Pval)))

}