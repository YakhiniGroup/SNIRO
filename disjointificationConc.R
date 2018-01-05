
disjointificationConc<- function(imprtdata,imprtgluc,imprtnormzvalue,imprtdiff1zvalue,imprtdiff2zvalue,imprtdiff3zvalue,imprtdiff4zvalue,imprtdiff5zvalue,cornum,sampType,numLs){

Zvalue<-rbind(imprtnormzvalue,imprtdiff1zvalue,imprtdiff2zvalue,imprtdiff3zvalue,imprtdiff4zvalue,imprtdiff5zvalue)
Zvalue<-sort(abs(Zvalue[,1]),decreasing=FALSE)
Zvalue<-as.matrix(Zvalue)

data<-turntodataset(imprtdata,imprtgluc)
colnames(imprtdata)<-paste(colnames(imprtdata),"nm",sep = "")
nir.diff1<-savitzkyGolay((as.matrix(data$NIR)),1,2,3)
colnames(nir.diff1)<-paste(colnames(nir.diff1),"d1",sep = "")
nir.diff2<-savitzkyGolay((as.matrix(data$NIR)),2,3,5)
colnames(nir.diff2)<-paste(colnames(nir.diff2),"d2",sep = "")
nir.diff3<-savitzkyGolay((as.matrix(data$NIR)),3,4,5)
colnames(nir.diff3)<-paste(colnames(nir.diff3),"d3",sep = "")
nir.diff4<-savitzkyGolay((as.matrix(data$NIR)),4,5,7)
colnames(nir.diff4)<-paste(colnames(nir.diff4),"d4",sep = "")
nir.diff5<-savitzkyGolay((as.matrix(data$NIR)),5,6,7)
colnames(nir.diff5)<-paste(colnames(nir.diff5),"d5",sep = "")

############## finds the distribution of correlation betwenn lambdas
#corlambdahist<-array(dim=928203)
#count<-1
#for(i in 1:nrow(imprtzvalue)){
#  print(count)
#  for(j in i:nrow(imprtzvalue)){
#      Li<-nir.diff1[,i]
#      Lj<-nir.diff1[,j]
#      corlambdahist[count]<-cor(Li,Lj)
#      count<-count+1
#  }
#}
#hist(corlambdahist, main= "Histogram of Lambdas 1st Derivative",ylim = c(0,1), col="red",labels = TRUE)

############# Disjointification
count=1
disjoint<-c(row.names(Zvalue)[1])
for(i in 1:nrow(Zvalue)){
  b<-FALSE
  print(i)
  for(j in 1:length(disjoint)){
    La<-(row.names(Zvalue))[i]
    Lb<-disjoint[j]
    
    if(grepl("nm",La)){
      dataLa<-imprtdata[,La]
    }
    if(grepl("d1",La)){
      dataLa<-nir.diff1[,La]
    }
        if(grepl("d2",La)){
          dataLa<-nir.diff2[,La]
        }
        if(grepl("d3",La)){
          dataLa<-nir.diff3[,La]
        }
        if(grepl("d4",La)){
          dataLa<-nir.diff4[,La]
       }
        if(grepl("d5",La)){
          dataLa<-nir.diff5[,La]
        }
    
    if(grepl("nm",Lb)){
      dataLb<-imprtdata[,Lb]
    }
    
    if(grepl("d1",Lb)){
      dataLb<-nir.diff1[,Lb]
    }
    if(grepl("d2",Lb)){
          dataLb<-nir.diff2[,Lb]
        }
        if(grepl("d3",Lb)){
          dataLb<-nir.diff3[,Lb]
        }
        if(grepl("d4",Lb)){
          dataLb<-nir.diff4[,Lb]
        }
        if(grepl("d5",Lb)){
          dataLb<-nir.diff5[,Lb]
        }
    
    if(abs(cor(dataLa,dataLb))>cornum){ #number of correlation that we want
      b<-TRUE
      break
    }
  }
  if(count==numLs){
    break
  }
  if(b==FALSE){
    disjoint<-c(disjoint,La)
    count=count+1
  }
  
}
disjoint<-as.matrix(disjoint)
disjoint<-cbind(disjoint,Zvalue[disjoint,])

if(nrow(disjoint)==numLs){
  write.csv(disjoint,paste(script.dir,"/Results","/","concat_disjointification",cornum,sampType,".csv",sep = ""))
}
return(disjoint)
}
