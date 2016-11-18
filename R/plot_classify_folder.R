#' Plot barplot for classified images in a folder
#'
#' @param path path to folder 
#' @param N number of classes, default: 7
#' @param col color of bars, either one or a vector of hex RGB characters 
#' @param cores number of cores to use in parallel (needs parallel package if cores>1)
#' @param method method for error bars ("sd", "minmax", "quartile")
#'
#' @return plots
#' @importFrom grDevices grey
#' @import utils
#' @export
plot_classify.folder<-function(path, N=7, cores=1 ,col=grDevices::grey(0.7),method="sd")
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(paste0("class",N))
  cat(paste(length(files),"files.\n"))
  
  if(cores>1)tables <- parallel::mclapply(files, read.table.helper, N=N, mc.cores=cores)
  if(cores>1)tables <- lapply(files, read.table.helper, N=N)
  tables<-array(unlist(tables),c(N,length(tables)))
  qu=NULL
  if (method=="quartile"){method="quantile";qu=c(.25,.75)}
  barplot_with_interval(tables,xlab="chromatin compaction levels",ylab="percentage",col=col,method=method,qu=qu)
}

read.table.helper<-function(file, N){
  table<-utils::read.table(paste0("class",N,"-n/",file,"-percent.txt"))
}