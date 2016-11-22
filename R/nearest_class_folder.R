#' Find all distances to next neighbour of all classes for folders
#'
#' @param path path to folder 
#' @param N number of classes, default: 7
#' @param voxelsize real size of voxels (in microns), if NULL (default), look in folder XYZmic
#' @param add if TRUE, only process images which have not been processed before (i.e. have been added to classN)
#' @param cores number of cores to use in parallel (needs parallel package if cores>1)
#' @export
#' @return nothing, results are in folder distances in RData format
#' @import bioimagetools stringr
#' 
nearestClassDistances.folder<-function(path, N=7, voxelsize=NULL, add=FALSE, cores=1)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(paste0("class",N))
  if(add)
    {
    files2<-list.files("distances")
    files<-comparefilelist(files,files2)
  }
  cat(paste(length(files),"files.\n"))
  
  
  
  if (length(files)==0)return()
  if(length(list.files("distances"))==0)dir.create("distances")
  
  if (cores>1)
  {
    ff<-length(files)
    cores1=min(floor(sqrt(cores)),ff)
    cores2=floor(cores/cores1)
  }
  
  if(cores>1)jobs <- parallel::mclapply(files, nearestClassDistances.files, N=N, voxelsize=voxelsize, cores=cores2, mc.preschedule=FALSE, mc.cores=cores1,  mc.allow.recursive = TRUE)
  if(cores==1)jobs <- lapply(files, nearestClassDistances.files, N=N, voxelsize=voxelsize)
  setwd(orig)
  
  #return(jobs)
}


nearestClassDistances.files<-function(file,N=7,voxelsize=NULL,cores=1)
{
  cat(file)
  cat("\n")
  test<-try({
    class<-readClassTIF(paste0("class",N,"/",file))
    if (is.null(voxelsize)){
      XYZ <- scan(paste0("XYZmic/",file,".txt"))
      voxelsize<-XYZ/dim(class)
    }
    mask<-readClassTIF(paste0("dapimask/",file))
    cat(".")
    distances<-bioimagetools::nearestClassDistances(class,voxelsize,classes=N,cores=cores,silent=TRUE)
    remove(class,mask)
    gc(verbose=FALSE)
    save(distances,file=paste0("distances/",file,".RData"))
  },silent=TRUE)
if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
else(cat(paste0(file," OK\n")))
}

