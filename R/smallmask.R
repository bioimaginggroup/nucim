mask.small<-function(f,color,n,cores=1)
{
  orig<-getwd()
  setwd(f)
  require(bioimagetools)
  if(cores>1)
  {
    require(parallel)
    options("mc.cores"=cores)
  }
  
  files<-list.files(color)
  cat(paste(length(files),"files.\n"))
  
  if(length(list.files(paste0(color,"mask")))==0)dir.create(paste0(color,"mask"))
  
  if(cores>1)jobs <- mclapply(files,mask.small.file,color=color,n=n)
  if(cores==1)jobs <- lapply(files,mask.small.file,color=color,n=n)

  setwd(orig)
}

mask.small.file<-function(file,color,n)
  {
  test<-try({
    
    mask<-readTIF(paste("dapimask/",file,sep=""))
    prot<-readTIF(paste0(color,"/",file))
    XYZ <- scan(paste0("XYZmic/",file,".txt"))
    
    xyzmic<-XYZ/dim(mask)
    xymic<-mean(xyzmic[1:2])
    pm<-median(prot)
    prot[mask==0]<-pm
    
    brush<-makeBrush(25,shape="gaussian",sigma=.1/xymic)
    prot1<-filterImage2d(prot,brush)
    
    prot1<-prot1-min(prot1)
    prot1<-prot1/max(prot1)
    
    prot13<-ifelse(prot1>.5,1,0)
    prot13<-prot13*mask
    prot14<-erode(prot13)
    prot4<-bwlabel3d(prot13,silent=TRUE)
    prot5<-cmoments3d(prot4,prot)
        
    which<-rev(order(prot5[,5]))[1:n]
    
    xi.mask<-array(0,dim(prot))
    for (i in 1:n)
      xi.mask<-xi.mask+ifelse(prot4==which[i],1,0)
    xi.mask<-dilate(xi.mask)
    xi.mask<-erode(xi.mask)
    writeTIF(xi.mask,file=paste0(color,"mask/",file),bps=8)
    remove(mask,prot,prot1,prot13,prot14,prot4,prot5,xi.mask)
    gc(verbose=FALSE)
  },silent=TRUE)
  print(file)
  print(test)
  
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}
