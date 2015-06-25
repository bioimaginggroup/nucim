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

mask.dapi<-function(img,mic,thresh="auto")
{
  mb<-apply(img,3,mean)
  mbr<-0.3*sum(range(mb))
  mbr<-which(mbr<mb)
  small<-min(mbr):max(mbr)
  dims0<-dim(img)
  blau<-img[,,small]
  dims<-dim(blau)
  blau<-blau-median(blau)
  blau[blau<0]<-0
  blau<-array(blau,dims)
  blau<-blau/max(blau)
  blau<-filterImage3d(blau,"var",4,1/3,silent=TRUE)
  
  xyzmic<-mic/dim(blau)
  xymic<-mean(xyzmic[1:2])
  
  brush<-makeBrush(25,shape="gaussian",sigma=.1/xymic)
  blau2<-filterImage2d(blau,brush)
  xx<-apply(blau2,1,mean)
  yy<-apply(blau2,2,mean)
  if(thresh=="auto")thresh<-c(.find.first.mode(xx),.find.first.mode(rev(xx)),.find.first.mode(yy),.find.first.mode(rev(yy)))
  
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
  
  mask<-fillHull(mask)
  return(mask)
}
