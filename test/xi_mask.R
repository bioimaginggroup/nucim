library(bioimagetools)

setwd("/home/schmid/projects/marion/C2C12Xpaint")
files<-sample(list.files("green"))
for (file in files)
  {
  try({
    print(file)
    mask<-readTIF(paste("dapimask/",file,sep=""))
    gb<-readTIF(paste("rgb/",file,sep=""))
    blau<-gb[,,2,]
    green<-gb[,,1,]
    prot<-readTIF(paste("green/",file,sep=""))
    #pm<-median(prot)
    #prot[mask==0]<-pm
 #   prot<-prot*2^16
#    storage.mode(prot)<-"integer"
    
    brush<-makeBrush(25,shape="gaussian",sigma=2)
    prot.smooth<-filterImage2d(prot,brush)
    
    dens<-density(prot[mask==1&prot>.1],from=.1,to=.5)
    dens.smooth<-density(prot.smooth[mask==1&prot.smooth>.1],from=.1,to=.5)
    
    thresh1 <- dens$x[dens$y<dens.smooth$y]
    thresh1 <- min(thresh1[thresh1>.1])
    thresh2 <- dens$x[dens$y>dens.smooth$y]
    thresh2 <- min(thresh2[thresh2>.2])

    prot.mask<-ifelse(prot.smooth>.5*(thresh1+thresh2),1,0)
    
    prot.objects<-bwlabel3d(prot.mask)
    prot.moments<-cmoments3d(prot.objects,prot)
    
    w<-sort(prot.moments[,5],decreasing=TRUE)
    w<-which(prot.moments[,5]>=w[4])
    prot.mask<-array(0,dim(prot.objects))
    for (i in w)
      prot.mask[prot.objects==i]<-1
    
    #rgb<-array(NA,c(dim(prot)[1:2],3,dim(prot)[3]))
    #rgb[,,1,]<-prot.mask
    #rgb[,,2,]<-green
    #rgb[,,3,]<-blau
    #writeImage(rgb,file=paste0("x_mask/",file),bits.per.sample=8L)

    writeTIF(prot.mask,file=paste0("x_mask/",file),bps=8L)
  })
}

if(0)
{
nr.xi<-2
    if(length(grep("mES",file))==0)nr.xi<-2
    if(dim(prot5)[1]==1)nr.xi<-1
    
    which<-rev(order(prot5[,5]))[1:nr.xi]
    xi<-prot5[which,2:4]
    
    xi.mask<-list()
    for (i in 1:nr.xi)
      xi.mask[[i]]<-(prot4==which[i])
    
    #   blue<-readImage(paste("rgb/",substr(file,0,18),"blue.tif",sep=""))
    
    #    test<-rgbImage(blue=blue,green=prot,red=xi.mask[[1]])
    #    writeImage(test,file=paste("xistbereich/1_",file,sep=""))
    #    if(nr.xi==2)
    #    {
    #      test<-rgbImage(blue=blue,green=prot,red=xi.mask[[2]])
    #     writeImage(test,file=paste("test_",file,sep=""))
    #    }
    
    save(xi.mask,file=paste("xist/",substr(file,0,17),".Rdata",sep=""))

}
