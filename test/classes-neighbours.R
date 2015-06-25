library(bioimagetools)

setwd("/home/schmid/projects/marion/C2C12Xpaint")
files<-sample(list.files("class7"))
for (file in files)
{
  try({
    class<-readTIF(paste0("class7/",file))
    class<-round(class*7)
    x<-dim(class)[1]
    y<-dim(class)[2]
    
    n1<-as.vector(c(class[1:(x-1),,],class[2:x,,],class[,1:(x-1),],class[,2:x,]))
    n2<-as.vector(c(class[2:x,,],class[1:(x-1),,],class[,2:x,],class[,1:(x-1),]))
              
    temp<-table(n1,n2)
    remove(n1,n2)
    gc()
    temp<-temp[-1,-1]
    pdf("class_neighbours.pdf")
    par(mfrow=c(4,2))
    for(i in 1:7)
      {
      plot(temp[i,],type="l",main=paste0("class ",i),axes=FALSE,ylab="",xlab="")
      axis(1)
      box()
      }
    dev.off()