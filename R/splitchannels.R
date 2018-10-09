#' Split channels into files and extracts size in microns
#'
#' @param file file name
#' @param channels e.g. c("red","green","blue")
#' @param rgb.folder folder with file
#' @param normalize boolean. Should we try to do some normalization?
#'
#' @return files in "./red/", "./green", "./blue" and "./XYZmic"
#' @export
#' @import bioimagetools
splitchannels.file<-function(file, channels, rgb.folder, normalize=FALSE)
{
test<-try({
img<-readTIF(paste(rgb.folder,file,sep="/"))

rr<-which(channels=="red")
gg<-which(channels=="green")
bb<-which(channels=="blue")

red=green=blue=0

D<-length(dim(img))
if (D==4)
{
  if(length(rr)>0)red<-img[,,rr,]
  if(length(gg)>0)green<-img[,,gg,]
  if(length(bb)>0)blue<-img[,,bb,]
}
if (D==3)
{
  Z<-dim(img)[3]
  byby=length(channels)
  if(length(rr)>0)red<-img[,,seq(rr,Z,by=byby)]  
  if(length(gg)>0)green<-img[,,seq(gg,Z,by=byby)]  
  if(length(bb)>0)blue<-img[,,seq(bb,Z,by=byby)]  
}


if(length(rr)>0)
{
if(normalize)
  {
  red<-red-find.mode(red)
  red[red<0]<-0
  red<-red-min(red)
  red<-red/max(red)
}
writeTIF(red,paste("red/",file,sep=""),bps=16L)
}

if(length(gg)>0)
{
  if(normalize){
green<-green-find.mode(green)
green[green<0]<-0
green<-green-min(green)
green<-green/max(green)
}
writeTIF(green,paste("green/",file,sep=""),bps=16L)
}


if(length(bb)>0)
{
if (normalize){
  bluecut<-blue-find.mode(blue)
bluecut[bluecut<0]<-0
bluecut<-bluecut-min(bluecut)
#blue<-blue-min(blue)
#blue<-blue/max(blue)
blue<-bluecut/max(bluecut)
}
writeTIF(blue,paste("blue/",file,sep=""),bps=16L)
}



Xmic<-attr(img,"x.resolution")
Ymic<-attr(img,"y.resolution")
Zmic<-as.numeric(attr(img,"slices"))*as.numeric(attr(img,"spacing"))
write(c(Xmic,Ymic,Zmic),file=paste("XYZmic/",file,".txt",sep=""))

remove(img,red,green,blue)
gc(verbose=FALSE)
},silent=TRUE)
if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
else(cat(paste0(file," OK\n")))
}

#' Split RGB images into channels and pixel size information
#'
#' @param path Path to root folder
#' @param channels Vector of channels in images
#' @param rgb.folder Folder with RGB images
#' @param cores Number of cores used in parallel, cores=1 implies no parallelization
#' @return Nothing, folders red, green, blue and XYZmic include separate channels and pixel size information
#' @export
#' @import bioimagetools
#' @examples splitchannels.folder("./")
#' 
splitchannels.folder<-function(path,channels=c("red","green","blue"),rgb.folder="rgb",cores=1)
{
  orig<-getwd()
  setwd(path)
  if(cores>1)
  {
    options("mc.cores"=cores)
  }
  
  files<-list.files(rgb.folder)
  cat(paste(length(files),"files.\n"))
  if (length(files)==0){cat("Nothing to do.\n");return()}              
  if(length(list.files("red"))==0)dir.create("red")
  if(length(list.files("blue"))==0)dir.create("blue")
 # if(length(list.files("blueorig"))==0)dir.create("blueorig")
  if(length(list.files("green"))==0)dir.create("green")
  if(length(list.files("XYZmic"))==0)dir.create("XYZmic")
  
  if(cores>1)jobs <- parallel::mclapply(files,splitchannels.file,channels,rgb.folder)
  if(cores==1)jobs <- lapply(files,splitchannels.file,channels,rgb.folder)
  setwd(orig)
}

#' Split RGB images into channels and pixel size information
#'
#' These functions are provided for compatibility with older version of
#' the nucim package.  They may eventually be completely
#' removed.
#' @param ... parameters for splitchannels.folder
#' @return Nothing, folders red, green, blue and XYZmic include seperate channels and pixel size information
#' @export
#' 
splitchannels<-function(...){
 .Deprecated("splitchannels.folder",package="nucim")
 splitchannels.folder(...)
 }