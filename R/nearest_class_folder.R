#' Find all distances to next neighbour of all classes for folders
#'
#' @param path path to folder 
#' @param N number of classes, default: 7
#' @param voxelsize real voxesize of image (in nanometers), if NULL (default), look in folder XYZmic
#' @param cores number of cores to use in parallel (needs parallel package)
#' @param method method for summarising distances, either "min" or "quantile"
#' @param qu quantile for method="quantile", default: 0.01
#' @export
#' @return nothing, results are in folder distances in RData format
#' @import bioimagetools
#' 
nearestClassDistances.folder<-function(path, N=7, voxelsize=NULL, cores=1, method="quantile", qu=0.01)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(paste0("class",N))
  cat(paste(length(files),"files.\n"))
  
  if (length(files)==0)return()
  if(length(list.files("distances"))==0)dir.create("distances")
  
  if(cores>1)jobs <- parallel::mclapply(files, nearestClassDistances.files, N=N, voxelsize=voxelsize, mc.preschedule=FALSE, mc.cores=cores)
  if(cores==1)jobs <- lapply(files, nearestClassDistances.files, N=N, voxelsize=voxelsize)

  if(cores>1)dist <- parallel::mclapply(files, ncd.helper, method=method, qu=qu, mc.preschedule=FALSE, mc.cores=cores)
  if(cores==1)dist <- lapply(files, ncd.helper, method=method, qu=qu, voxelsize=voxelsize)
  
  dist<-array(unlist(dist),c(length(files),N,N))
  
  mfrow=ceiling(N^(2/3))
  mfrow=c(ceiling(N/mfrow),mfrow)
  graphics::par(mfrow=mfrow)
  m<-apply(dist,2:3,mean)
  for (i in 1:N)
    graphics::barplot(m[i,])

  

  setwd(orig)
  
  
  #return(jobs)
}

ncd.helper<-function(file, method="quantile", qu=0)
{
  load(file)
  N<-length(distances)
  if (method=="quantile")distances<-lapply(distances,ncd.helper.qu,qu=qu)
  if (method=="min")distances<-lapply(distances,ncd.helper.min)
  distances<-array(unlist(distances),c(N,N))
  return(distances)
}
ncd.helper.qu<-function(dist,qu)
{
  dist<-lapply(dist,quantile,probs=qu)
  return(dist)
}
ncd.helper.min<-function(dist)
{
  dist<-lapply(dist,min)
  return(dist)
}

nearestClassDistances.files<-function(file,N=7,voxelsize=NULL,cores=1)
{
  test<-try({
    class<-readClassTIF(paste0("class",N,"/",file))
    if (is.null(voxelsize)){
      XYZ <- scan(paste0("XYZmic/",file,".txt"))
      voxelsize<-XYZ/dim(class)
    }
    mask<-readClassTIF(paste0("dapimask/",file))
    distances<-nearestClassDistances(class,voxelsize,classes=N,cores=cores)
    remove(class,mask)
    gc(verbose=FALSE)
    save(distances,file=paste0("distances/",file,".RData"))
  },silent=TRUE)
if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
else(cat(paste0(file," OK\n")))
}

