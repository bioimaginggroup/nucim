#' automatic DAPI masks for folder
#'
#' @param path path to folder with DAPI
#' @param folder folder with DAPI images
#' @param cores number of cores to use in parallel (need paralle package)
#' @export
#' @return nothing, results are in folder dapimask
dapimask.folder<-function(path, folder="blue", cores=1)
{
  orig<-getwd()
  setwd(path)
  
  library(bioimagetools)
  if (cores>1)
  {
    require(parallel)
  }
  files<-list.files(folder)
  cat(paste(length(files),"files.\n"))
  
  if (length(files)==0)return()
  if(length(list.files("dapimask"))==0)dir.create("dapimask")
  
  
  if(cores>1)jobs <- mclapply(files, dapimask, folder=folder, mc.preschedule=FALSE, mc.cores=cores)
  if(cores==1)jobs <- lapply(files, dapimask, folder=folder)
  setwd(orig)
  #return(jobs)
}

