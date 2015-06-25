library(bioimagetools)

setwd("/home/schmid/projects/marion/C2C12Xpaint")
files<-sample(list.files("blue"))
for (file in files)
{
mask<-readTIF(paste("dapimask/",file,sep=""))
blau<-readTIF(paste("blue/",file,sep=""))#[,,2,]

brush<-makeBrush(25,shape="gaussian",sigma=5)
v<-filterImage2d(blau,filter=brush)

par(mfrow=c(1,1))
plot(density(as.vector(v)))
thresh<-locator()[[1]][1]
nucmask<-ifelse(v>thresh,1,0)

n<-3
brush<-makeBrush(2*n-1,shape='box')
nucmask<-erode(nucmask,brush)

par(mfrow=c(2,2))
image(blau[,,15])
image(ifelse(nucmask[,,15]==1,NA,1),add=TRUE)
image(blau[,,25])
image(ifelse(nucmask[,,25]==1,NA,1),add=TRUE)
image(blau[,,35])
image(ifelse(nucmask[,,35]==1,NA,1),add=TRUE)
image(blau[,,45])
image(ifelse(nucmask[,,45]==1,NA,1),add=TRUE)

mask0<-bwlabel3d(1-nucmask,silent=TRUE)
image(t(mask0[512:1,,15]),zlim=c(0,33))
image(t(mask0[512:1,,25]),zlim=c(0,33))
image(t(mask0[512:1,,35]),zlim=c(0,33))
image(t(mask0[512:1,,45]),zlim=c(0,33))

mask1<-cmoments3d(mask0,mask)
print(sort(mask1[,5]))

mask<-readTIF(paste("dapimask/",file,sep=""))
for (w in which(mask1[,5]>10000))
  mask[mask0==w]<-0

par(mfrow=c(2,2))
image(blau[,,25])
image(ifelse(mask[,,25]==1,NA,1),add=TRUE)
image(blau[,,35])
image(ifelse(mask[,,35]==1,NA,1),add=TRUE)
image(blau[,,15])
image(ifelse(mask[,,15]==1,NA,1),add=TRUE)
image(blau[,,45])
image(ifelse(mask[,,45]==1,NA,1),add=TRUE)

writeTIF(mask,paste("numask/",file,sep=""))
}
