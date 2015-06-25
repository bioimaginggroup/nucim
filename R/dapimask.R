dapimask<-function(f,cores=1)
  {
orig<-getwd()
setwd(f)

  library(bioimagetools)
  if (cores>1)
    {
    library(parallel)
    options("mc.cores"=cores)
  }
files<-list.files("blue")
cat(paste(length(files),"files.\n"))

if(length(list.files("dapimask"))==0)dir.create("dapimask")


if(cores>1)jobs <- mclapply(files, dapimask.file, mc.preschedule=FALSE)
if(cores==1)jobs <- lapply(files, dapimask.file)
setwd(orig)
}

dapimask.file<-function(file){
  test<-try({
  blau<-readTIF(paste("blue/",file,sep=""))
  mb<-apply(blau,3,mean)
  mbr<-0.3*sum(range(mb))
  mbr<-which(mbr<mb)
  small<-min(mbr):max(mbr)
  dims0<-dim(blau)
  blau<-blau[,,small]
  dims<-dim(blau)
  blau<-blau-median(blau)
  blau[blau<0]<-0
  blau<-array(blau,dims)
  blau<-blau/max(blau)
  blau<-filterImage3d(blau,"var",4,1/3,silent=TRUE)
  
  XYZ <- scan(paste0("XYZmic/",file,".txt"))
  xyzmic<-XYZ/dim(blau)
  xymic<-mean(xyzmic[1:2])
  
  brush<-makeBrush(25,shape="gaussian",sigma=.1/xymic)
  blau2<-filterImage2d(blau,brush)
  xx<-apply(blau2,1,mean)
  yy<-apply(blau2,2,mean)
  thresh<-c(.find.first.mode(xx),.find.first.mode(rev(xx)),.find.first.mode(yy),.find.first.mode(rev(yy)))
  
  b<-blau>median(thresh/2)
  #b<-blau>quantile(blau,.8)
  b2<-array(0,dims0)
  b2[,,small]<-array(as.integer(b),dim(b))
  n<-5
  mask<-1-outside(b2,0,n)
  brush<-makeBrush(2*n-1,shape='box')
  mask<-erode(mask,brush)
  
  mask0<-bwlabel3d(mask,silent=TRUE)
  mask1<-cmoments3d(mask0,mask)
  
  which<-rev(order(mask1[,5]))[1]
  
  mask<-ifelse(mask0==which,1,0)
  
  if(0)
  { 
    z<-25
    bb<-mask[,,z]-.5
    bb<-bb*blau[,,z]
    image(bb)
  }
  
  mask<-fillHull(mask)
  
  #mask0<-1-outside(b,0,15)
  #mask<-array(0,dims0)
  #mask[,,small]<-array(as.integer(mask0),dim(mask0))
  writeTIF(mask,paste("dapimask/",file,sep=""),bps=8)
  remove(blau,mask,mbr,b2)
  gc(verbose=FALSE)
},silent=TRUE)
if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
else(cat(paste0(file," OK\n")))
}  

.find.first.mode<-function(x)
{
  s<-sd(diff(x))
  i<-1
  go<-TRUE
  while(go)
  {
    i<-i+1
    if(x[i]-x[i-1]<(-1.96*s))go<-FALSE
  }
  return(x[i-1])
}