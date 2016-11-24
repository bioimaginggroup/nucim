#' Find spots using information from two channels for folder
#'
#' @param path path to folder
#' @param size size of img in microns, if size and voxelsize are NULL, size is determined from folder XYZmic
#' @param voxelsize size of voxel in microns
#' @param thresh.offset Thresh offest used in EBImage::thresh() 
#' @param min.sum.intensity spots smaller than min.sum.intensity are ignored
#' @param max.distance use only spots with distance to other color spot smaller than max.distance
#' @param use.brightest Logical; use only brightest in max.distance?
#' @param max.spots maximum of spots (per channel), only when use brightest=TRUE
#' @param full.voxel Logical; output contains full voxel instead of rgb intensities
#' @param output output folder
#' @param cores number of cores we can use of parallel computing (needs parallel package if cores>1)
#'
#' @return RGB image with spots will be written to output folder
#' @export
#' @import bioimagetools fields EBImage
#'
spots.combined.folder<-function(path, size=NULL, voxelsize=NULL, thresh.offset=0.1, min.sum.intensity=2,max.distance=0.5, use.brightest=FALSE,  max.spots=2, full.voxel=FALSE, output="markers", cores=1)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(paste0(path,"/red"))
  cat(paste(length(files),"files.\n"))
  
  if (length(files)==0)return()
  if(length(list.files(paste0(output,"_red")))==0)dir.create(paste0(output,"_red"))
  if(length(list.files(paste0(output,"_green")))==0)dir.create(paste0(output,"_green"))
        
  if(cores>1)jobs <- parallel::mclapply(files, spots.combined.file, size=size, voxelsize=voxelsize, folder=path, thresh.offset=thresh.offset, min.sum.intensity=min.sum.intensity,max.distance=max.distance, use.brightest=use.brightest,  max.spots=max.spots, full.voxel=full.voxel, output=output, mc.preschedule=FALSE, mc.cores=cores)
  if(cores==1)jobs <- lapply(files, spots.combined.file, size=size, voxelsize=voxelsize, folder=path, thresh.offset=thresh.offset, min.sum.intensity=min.sum.intensity,max.distance=max.distance, use.brightest=use.brightest,  max.spots=max.spots, full.voxel=full.voxel, output=output)
  setwd(orig)
  #return(jobs)
}


#' Find spots using information from two channels
#'
#' @param file File name
#' @param folder Folder
#' @param size size of img in microns, if size and voxelsize are NULL, size is determined from folder XYZmic
#' @param voxelsize size of voxel in microns
#' @param thresh.offset Thresh offest used in EBImage::thresh() 
#' @param min.sum.intensity spots smaller than min.sum.intensity are ignored
#' @param max.distance use only spots with distance to other color spot smaller than max.distance
#' @param use.brightest Logical; use only brightest in max.distance?
#' @param max.spots maximum of spots (per channel), only when use brightest=TRUE
#' @param full.voxel Logical; output contains full voxel instead of rgb intensities
#' @param output output folder
#'
#' @return RGB image with spots will be written to output folder
#' @export
#' @import bioimagetools fields EBImage
#'
spots.combined.file<-function(file, size=NULL, voxelsize=NULL, folder="./", thresh.offset=0.1, min.sum.intensity=2,max.distance=0.5, use.brightest=FALSE,  max.spots=2, full.voxel=FALSE, output="markers")
{
  oldwd=getwd()
  setwd(folder)
  #try({
  mask<-bioimagetools::readTIF(paste0("dapimask/",file))
  red<-bioimagetools::readTIF(paste0("red/",file))
  green<-bioimagetools::readTIF(paste0("green/",file))
  if (is.null(size)&is.null(voxelsize)){
    size<-scan(paste0("XYZmic/",file,".txt"))}
  
  result <- spots.combined(red, green, mask, size, thresh.offset=thresh.offset, min.sum.intensity=min.sum.intensity,max.distance=max.distance, use.brightest=use.brightest,  max.spots=max.spots, full.voxel=full.voxel)
    
  writeTIF(result$red,file=paste0(output,"_red/",file),bps=16L,reduce=TRUE)
  writeTIF(result$green,file=paste0(output,"_green/",file),bps=16L,reduce=TRUE)
  setwd(oldwd)
}

#' Find spots using information from two channels
#'
#' @param red image
#' @param green image
#' @param mask image mask
#' @param size size of img in microns 
#' @param voxelsize size of voxel in microns
#' @param thresh.offset Thresh offest used in EBImage::thresh() 
#' @param min.sum.intensity spots smaller than min.sum.intensity are ignored
#' @param max.distance use only spots with distance to other color spot smaller than max.distance
#' @param use.brightest Logical; use only brightest in max.distance?
#' @param max.spots maximum of spots (per channel), only when use brightest=TRUE
#' @param full.voxel Logical; output contains full voxel instead of rgb intensities
#'
#' @return RGB image with spots will be written to output folder
#' @export
#' @import bioimagetools fields EBImage
#'
spots.combined<-function(red, green, mask, size=NULL, voxelsize=NULL, thresh.offset=0.1, min.sum.intensity=2,max.distance=0.5, use.brightest=FALSE,  max.spots=NA, full.voxel=FALSE)
{
  if (is.null(size)&is.null(voxelsize)){stop("Either size or voxelsize is required")}
  if(is.null(voxelsize))voxelsize<-size/dim(img)
  
  red.s<-bioimagetools::spots(red, mask, thresh.offset, min.sum.intensity, zero=NA, return="l")
  green.s<-bioimagetools::spots(green, mask, thresh.offset, min.sum.intensity, zero=NA, return="l")

  red.c<-bioimagetools::cmoments3d(red.s,red)
  green.c<-bioimagetools::cmoments3d(green.s,green)
  
  if (is.null(dim(red.c)))red.c<-rbind(array(red.c,c(1,length(red.c))),c(0,0,0))
  if (is.null(dim(green.c)))green.c<-rbind(array(green.c,c(1,length(green.c))),c(100,100,10))
  
  xyz<-size/dim(mask)
  for (i in 1:dim(red.c)[1])red.c[i,2:4]<-red.c[i,2:4]*xyz
  for (i in 1:dim(green.c)[1])green.c[i,2:4]<-green.c[i,2:4]*xyz
  
  rdist <- function(x,y)
  {
    I<-dim(x)[1]
    J<-dim(y)[1]
    index.x<-rep(1:I,each=J)
    index.y<-rep(1:J,I)
    x<-x[index.x,]
    y<-y[index.y,]
    d<-array(sqrt(apply((x-y)^2,1,sum)),c(J,I))
    return(t(d))
  }
  rd<-rdist(red.c[,2:4],green.c[,2:4])
  
  potential <- which(rd<max.distance,arr.ind=TRUE)
  potential.red <- potential[,1]
  potential.green <- potential[,2]
  
  if (use.brightest)
  {
    
    table(potential.red)->tt
    kill<-as.numeric(names(which(tt>1)))
    if (length(kill)>0)
    {
      weg=c()
      for (i in kill)
      {
        pog<-potential.green[potential.red==i]
        pog0<-which(green.c[pog,5]==max(green.c[pog,5]))
        for (j in pog[-pog0])
        {
          weg<-c(weg,which(potential.red==i&potential.green==j))
        }  
      }
      potential.red<-potential.red[-weg]
      potential.green<-potential.green[-weg]
    }
    
    table(potential.green)->tt
    kill<-as.numeric(names(which(tt>1)))
    if (length(kill)>0)
    {
      weg<-c()
      for (i in kill)
      {
        pog<-potential.red[potential.green==i]
        pog0<-which(red.c[pog,5]==max(red.c[pog,5]))
        for (j in pog[-pog0])
        {
          weg<-c(weg,which(potential.green==i&potential.red==j))
        }  
      }
      potential.red<-potential.red[-weg]
      potential.green<-potential.green[-weg]
    }
    
    if (length(potential.green)>max.spots)
    {
      d=c()
      for (i in 1:length(potential.green))
        d=c(d,rd[potential.red[i],potential.green[i]])
      dd<-min(d)
      dd<-min(d[-which(d==dd)])
      d<-which(d<=dd)
      potential.green<-potential.green[d]
      potential.red<-potential.red[d]
    }
  }
  else
  {
    potential.red<-unique(potential.red)
    potential.green<-unique(potential.green)
  }
  
  
  labels.red<-red.c[potential.red,1]
  labels.green<-green.c[potential.green,1]
  
  new.red<-array(0,dim(mask))
  new.green<-array(0,dim(mask))
  red.s[is.na(red.s)]=0
  green.s[is.na(green.s)]=0
  
  if (!full.voxel)  for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-red[red.s==labels.red[i]]
  if (!full.voxel)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-green[green.s==labels.green[i]]
  if (full.voxel)   for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-1
  if (full.voxel)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-1

  return(list("red"=new.red,"green"=new.green))
}