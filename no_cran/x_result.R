library(bioimagetools)

setwd("/home/schmid/projects/marion/C2C12Xpaint")

cx<-read.table("x_results/class_x.txt")
ca<-read.table("x_results/class_all.txt")

summary(ca)
par(mfrow=c(2,3))
for (i in 1:6)
  barplot(as.numeric(cx[i,3:9]))

summary(cx)
par(mfrow=c(2,3))
for (i in 1:23)
if (cx[i,3]=="xi")barplot(as.numeric(cx[i,4:10]),main="xi")

par(mfrow=c(2,3))
for (i in 1:23)
  if (cx[i,3]=="xa")barplot(as.numeric(cx[i,4:10]),main="xa")

par(mfrow=c(2,3))
for (i in 1:23)
  if (cx[i,3]=="na")barplot(as.numeric(cx[i,4:10]),main="na")

cxm<-as.matrix(cx[,4:10])
for (i in 1:23)
  cxm[i,]<-cxm[i,]/sum(cxm[i,])
cam<-as.matrix(ca[,2:8])
for (i in 1:6)
  cam[i,]<-cam[i,]/sum(cam[i,])

xi<-apply(cxm[cx[,3]=="xi",],2,mean)
xa<-apply(cxm[cx[,3]=="xa",],2,mean)
na<-apply(cxm[cx[,3]=="na",],2,mean)
all<-apply(cam,2,mean)

png("x_result.png")
names(all)=1:7
barplot(all/sum(all),axes=FALSE,ylim=c(0,.5))
axis(2,las=2)

names(xi)<-names(xa)<-names(na)<-""
barplot(xi/sum(xi),add=TRUE,col="blue",density=30,axes=FALSE)
barplot(xa/sum(xa),add=TRUE,col="orange",density=30,axes=FALSE,angle=225)
#barplot(na/sum(na),add=TRUE,col="black",density=20,axes=FALSE,angle=90)
dev.off()


