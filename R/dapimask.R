#' Mask DAPI in kernel
#'
#' @param img DAPI channel image (3d)
#' @param size size of img in microns 
#' @param voxelsize size of voxel in microns
#' @param thresh threshold for intensity. Can be "auto": function will try to find automatic threshold
#' @param cores number of cores available for parallel computing
#' @param silent Keep silent?
#' 
#' @return mask image, array with same dimension as img.
#' @export
#'
#' @import EBImage bioimagetools stats
#' 
dapimask<-function(img, size=NULL, voxelsize=NULL, thresh="auto", silent=TRUE, cores=1)
{
  if (is.null(size)&is.null(voxelsize)){stop("Either size or voxelsize is required")}
  if (length(dim(img))==3)
     {
    mb<-apply(img,3,mean)
  mbr<-0.8*quantile(mb,0.01)+0.2*quantile(mb,0.99)
  mbr<-which(mbr<mb)
  small<-min(mbr):max(mbr)
  dims0<-dim(img)
  blau<-img[,,small]
  dims<-dim(blau)
  }
  else
    {
      blau=img
      dims<-dim(blau)
      }
  blau<-blau-median(blau)
  blau[blau<0]<-0
  blau<-array(blau,dims)
  blau<-blau/max(blau)
  blau<-bioimagetools::filterImage3d(blau,"var",4,1/3,silent=silent)
  
  if(is.null(voxelsize))voxelsize<-size/dim(img)
  xymic<-mean(voxelsize[1:2])
  
  brush<-EBImage::makeBrush(25,shape="gaussian",sigma=.1/xymic)
  blau2<-EBImage::filter2(blau,brush)
  xx<-apply(blau2,1,mean)
  yy<-apply(blau2,2,mean)
  temp<-list("a"=xx,"b"=rev(xx),"c"=yy,"d"=rev(yy))
  if(thresh=="auto"){
    if(cores>1){thresh<-unlist(parallel::mclapply(temp,find.first.mode,mc.cores=4))}else{thresh<-unlist(lapply(temp,find.first.mode))}
  thresh=median(thresh, na.rm=TRUE)
    }
  b<-blau>median(thresh/2)
  #b<-blau>quantile(blau,.8)
  if (length(dim(img))==3)
    {
    b2<-array(0,dims0)
  b2[,,small]<-array(as.integer(b),dim(b))
  }
  else
  {
    b2=b
  }
  n<-5
  mask<-1-bioimagetools::outside(b2,0,n)
  brush<-makeBrush(2*n-1,shape='box')
  mask<-erode(mask,brush)
  
  if (length(dim(img))==3)
  {
    mask0<-bioimagetools::bwlabel3d(mask)
  mask1<-bioimagetools::cmoments3d(mask0,mask)
  which<-rev(order(mask1[,5]))[1]
  }
  else
  {
    mask0<-EBImage::bwlabel(mask)
    mask1<-EBImage::computeFeatures(mask0[,,1,1],mask[,,1,1],methods.ref="computeFeatures.basic")
    which<-rev(order(mask1[,6]))[1]
  }
  
  mask<-ifelse(mask0==which,1,0)
  
  mask<-EBImage::fillHull(mask)
  
  return(mask)
}

find.first.mode<-function(x)
{
  s<-stats::sd(diff(x))
  i<-1
  go<-TRUE
  try({
    while(go)
  {
    i<-i+1
    if(x[i]-x[i-1]<(-1.96*s))go<-FALSE
    }
  },silent=TRUE)
  return(x[i-1])
}
