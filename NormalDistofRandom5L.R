normaldist<-function(err5,randerr5L,imprtsampval,tt){

  histo=c()
for (i in 1:1000){
  imprtsampvalrand<-as.matrix((sample(as.matrix(imprtsampval))))
  histo<-c(histo,(100*((sum(((imprtsampvalrand-imprtsampval)/imprtsampval)^2))^0.5)/nrow(imprtsampval)))
}
rr5pval=(sum(histo<err5)+1)/1000
pdf(file=paste(script.dir,"/Results/Dist",tt,sampState,sampType,".pdf",sep=""))
hist(histo,xlim=c(0,0.5+max(histo,randerrtrain5L,randerrtest5L)),xlab="Total Relative Error [%]",main = "Density histogram of Relative Error for Randomized Vectors",sub=paste(tt,sampType,sampState),cex.sub=0.75,cex.main=0.9)
points(err5,50,col="black",pch= 1)
points(randerr5L,50,col="red",pch= 8)
legend("topleft", legend = c("Actual","Random"),col = c("black","red") ,inset = .01,pch = c(1,8),cex = 0.75)
legend("topright", legend = c("5"),horiz = TRUE ,inset = .01,title = "Number of Ls",col = c("black"),pch =NULL,cex = 0.75)
legend("bottomleft", legend = c(rr5pval),horiz = TRUE ,inset = .01,title = "Pval",col = c("black"),pch =NULL, cex = 0.5)
dev.off()

}
