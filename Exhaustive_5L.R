library(data.table)
library(ChemometricsWithR)
library(prospectr)
library(corpcor)
library(fields)
library(BioPhysConnectoR)
Exhaustive_5L<-function(imprtdata,imprtgluc,disjoint,disjoint_zvalue,zvnum,sampType){
  i<-1
  while((as.numeric(disjoint_zvalue)[i]>=zvnum)&(i<length(disjoint))){ # finds the amount of elements in disjoint that have a zvalue greater than the number indicated
    i=i+1
  }
  numVar<-i # The number of Wavelengths to calculate
  
  data<-matrix(data=NA, nrow=nrow(imprtdata), ncol = numVar)
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
  
  for(i in 1:numVar){
    if(grepl("nm",disjoint[i])){
      data[,i]<-imprtdata[,disjoint[i]]
    }
    if(grepl("d1",disjoint[i])){
      data[,i]<-nir.diff1[,disjoint[i]]
    }
    if(grepl("d2",disjoint[i])){
      data[,i]<-nir.diff2[,disjoint[i]]
    }
    if(grepl("d3",disjoint[i])){
      data[,i]<-nir.diff3[,disjoint[i]]
    }
    if(grepl("d4",disjoint[i])){
      data[,i]<-nir.diff4[,disjoint[i]]
    }
    if(grepl("d5",disjoint[i])){
      data[,i]<-nir.diff5[,disjoint[i]]
    }
  }
  colnames(data)<-disjoint[1:numVar]
  row.names(data)<-row.names(imprtdata)
  colnames(imprtgluc)<-c("VAR")
  data<- turntodataset(data,imprtgluc)
  
  numcols<-ncol(data$NIR)
  Ls<-5

  no_cores <- detectCores() - 1
  cl<-makeCluster(no_cores)
  registerDoParallel(cl)
  start.time <- Sys.time()
  comb <- function(answer1, answer2) if (answer1$error < answer2$error) answer1 else answer2
  opts <- list(chunkSize=2)
  
  answer <- foreach(i=1:(numcols-4), .combine='comb',.packages=c('foreach'),.options.nws=opts) %dopar% {
    comb <- function(answer1, answer2) if (answer1$error < answer2$error) answer1 else answer2
    foreach(j=(i+1):(numcols-3),.combine='comb') %do%{
      foreach(k=(j+1):(numcols-2),.combine='comb') %do%{ 
        foreach(l=(k+1):(numcols-1),.combine='comb') %do%{
          foreach(m=(l+1):(numcols),.combine='comb') %do%{ 
            lm<-lm(data$VAR~data$NIR[,c(i,j,k,l,m)]-1)
            C<-as.numeric(lm$coefficients)
            S.pred<-as.matrix(lm$fitted.values)
            adjR2<-summary.lm(lm)$adj.r.squared
            spcor<-cor(S.pred,data$VAR, method = "spearman")
            error<-sqrt(sum(((S.pred-data$VAR)/data$VAR)^2))
            data.frame(i,j,k,l,m,error,spcor)
          }
        }
      }
    }
  }  
  stopCluster(cl)
  end.time <- Sys.time()
  time.taken <- (end.time - start.time)
  print(time.taken)
  
  listerrorrate<-cbind(t(as.matrix(colnames(data$NIR[,as.matrix(answer[order(answer$error)[1],][1,1:5])]))),as.matrix(answer[order(answer$error)[1],][1,6:7]),time.taken)
  NIR.pred<-data$NIR[,listerrorrate[1,c(1:5)]]
  lm<-lm(data$VAR~NIR.pred-1)
  C<-as.numeric(lm$coefficients)
  S.pred<-as.matrix(lm$fitted.values)
  write.csv(listerrorrate,paste(script.dir,"/Results","/",sampType,"results.csv",sep = ""))
  return(list(S.pred,listerrorrate,C))
}