#' Automatic DAPI mask segmentation for folder
#'
#' @param path path to folder with DAPI
#' @param folder folder with DAPI images
#' @param voxelsize real size of voxel (in microns), if NULL (default), look in folder XYZmic
#' @param size real size of image (in microns), if NULL (default), look in folder XYZmic
#' @param cores number of cores to use in parallel (need parallel package)
#' @export
#' @return nothing, results are in folder dapimask
#' 
dapimask.folder<-function(path, folder="blue", voxelsize=NULL, size=NULL, cores=1)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(folder)
  cat(paste(length(files),"files.\n"))
  
  if (length(files)==0)return()
  if(length(list.files("dapimask"))==0)dir.create("dapimask")
  
  if(cores>1)jobs <- parallel::mclapply(files, dapimask.file, folder=folder, voxelsize=voxelsize, size=size, silent=TRUE, mc.preschedule=FALSE, mc.cores=cores)
  if(cores==1)jobs <- lapply(files, dapimask.file, folder=folder, voxelsize=voxelsize, size=size, silent=TRUE)
  setwd(orig)
}

