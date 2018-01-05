
results20testgroup<- function(imprttest20,imprttest20gluc,imprtdata,imprtgluc,imprtL){

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

Lambdas<-matrix(data=NA, nrow=nrow(imprttest20gluc), ncol = length(imprtL))
data<-matrix(data=NA, nrow=nrow(imprtdata), ncol = length(imprtL))

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

for (i in 1:length(imprtL)){
  if(grepl("nm",imprtL[i])){
    Lambdas[,i]<-imprttest20[,imprtL[i]]
    data[,i]<-imprtdata[,imprtL[i]]
  }
  if(grepl("d1",imprtL[i])){
    Lambdas[,i]<-nir20.diff1[,imprtL[i]]
    data[,i]<-nir.diff1[,imprtL[i]]
  }
  if(grepl("d2",imprtL[i])){
    Lambdas[,i]<-nir20.diff2[,imprtL[i]]
    data[,i]<-nir.diff2[,imprtL[i]]
  }
  if(grepl("d3",imprtL[i])){
    Lambdas[,i]<-nir20.diff3[,imprtL[i]]
    data[,i]<-nir.diff3[,imprtL[i]]
  }
  if(grepl("d4",imprtL[i])){
    Lambdas[,i]<-nir20.diff4[,imprtL[i]]
    data[,i]<-nir.diff4[,imprtL[i]]
  }
  if(grepl("d5",imprtL[i])){
    Lambdas[,i]<-nir20.diff5[,imprtL[i]]
    data[,i]<-nir.diff5[,imprtL[i]]
  }
}

colnames(Lambdas)<-imprtL[1:length(imprtL)]
row.names(Lambdas)<-row.names(imprttest20)
colnames(data)<-imprtL[1:length(imprtL)]
row.names(data)<-row.names(imprtdata)
colnames(imprtgluc)<-c("VAR")
data<- turntodataset(data,imprtgluc)

NIR.new<-data$NIR
C.new<-pseudoinverse(NIR.new)%*%data$VAR
S.pred<-Lambdas%*%C.new
relerrorrate<-sqrt(sum(((S.pred-imprttest20gluc)/imprttest20gluc)^2))
lm<-lm(as.matrix(imprttest20gluc)~ S.pred-1)
pval<-(summary.lm(lm))$coefficients[,4]
spcor<-cor(S.pred,imprttest20gluc, method = "spearman")
return(list(S.pred,relerrorrate,spcor,pval,C.new))

plot(S.pred, as.matrix(imprttest20gluc), xlab = "S.pred", ylab= "S.Real", main = paste(length(imprtL), "Ls- Spearman graph",sep = ""),sub= paste("Spearman cor is:",spcor),xlim=c(0, 8), ylim=c(0, 8))
abline(a=0, b=1, col="red")
#############

}


