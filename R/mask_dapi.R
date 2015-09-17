#' Mask DAPI in kernel
#'
#' @param img DAPI channel image (3d)
#' @param mic vector of dimensions of img in microns
#' @param thresh threshold for intensity. Can be "auto": function will try to find automatic threshold
#'
#' @return mask image, same dimension as img.
#' @export
#'
#' @import EBImage bioimagetools
#' 
mask.dapi<-dapimask<-function(img,mic,thresh="auto")
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
  blau<-bioimagetools::filter(blau,"var",4,1/3,silent=TRUE)
  
  xyzmic<-mic/dim(blau)
  xymic<-mean(xyzmic[1:2])
  
  brush<-makeBrush(25,shape="gaussian",sigma=.1/xymic)
  blau2<-EBImage::filter2(blau,brush)
  xx<-apply(blau2,1,mean)
  yy<-apply(blau2,2,mean)
  temp<-list("a"=xx,"b"=rev(xx),"c"=yy,"d"=rev(yy))
  if(thresh=="auto")if(require(parallel)){thresh<-unlist(mclapply(temp,find.first.mode,mc.cores=4))}else{thresh<-unlist(lapply(temp,find.first.mode))}
  
  b<-blau>median(thresh/2)
  #b<-blau>quantile(blau,.8)
  b2<-array(0,dims0)
  b2[,,small]<-array(as.integer(b),dim(b))
  n<-5
  mask<-1-outside(b2,0,n)
  brush<-makeBrush(2*n-1,shape='box')
  mask<-erode(mask,brush)
  
  mask0<-bwlabel3d(mask)
  mask1<-cmoments3d(mask0,mask)
  
  which<-rev(order(mask1[,5]))[1]
  
  mask<-ifelse(mask0==which,1,0)
  
  mask<-fillHull(mask)
  return(mask)
}

find.first.mode<-function(x)
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
