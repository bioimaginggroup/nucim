library(bioimagetools)
library(fields)

setwd("/home/schmid/projects/marion/C2C12Xpaint")
files<-(list.files("blue"))
for (file in files)
{
  try({
  mask<-readTIF(paste0("dapimask/",file))
  xmask<-readTIF(paste0("x_mask/",file))
  xmask<-round(xmask)
  xmask<-bwlabel3d(xmask,silent=TRUE)
  class<-readTIF(paste0("class7/",file))
  class<-round(class*7)
  png(paste0("x_results/",file,".png"))
  par(pty="s")
  cm<-apply(class,1:2,mean)
  image(cm,col=grey(c(1,seq(0,1,by=.01))),axes=FALSE)
  box()
  xm<-apply(xmask*mask,1:2,max)
  image.plot(ifelse(xm==0,NA,xm),add=TRUE)
  dev.off()
  
  t0<-table.n(as.vector(class[mask==1]),7)
  write(c(file,t0),file="x_results/class_all.txt",append=TRUE,ncolumns=8)
  for (i in 1:10)
    {
    t<-table.n(as.vector(class[mask==1&xmask==i]),7)
    write(c(file,i,t),file="x_results/class_x.txt",append=TRUE,ncolumns=9)
    }
  })
}
