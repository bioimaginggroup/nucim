#' Plot barplot for classified images in a folder
#'
#' @param path path to folder 
#' @param N number of classes, default: 7
#' @param cores number of cores to use in parallel (needs parallel package if cores>1)
#'
#' @return plots
#' @export
plotclassify.folder<-function(path, N=7, cores=1)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files(paste0("class",N))
  cat(paste(length(files),"files.\n"))
  
  if(cores>1)tables <- parallel::mclapply(files, read.table.helper, N=N, mc.cores=cores)
  if(cores>1)tables <- lapply(files, read.table.helper, N=N)
  tables<-array(unlist(tables),c(N,length(tables)))
  barplot.with.ci(tables,xlab="classes",ylab="percentage")
}

read.table.helper<-function(file, N){
  table<-utils::read.table(paste0("class",N,"-n/",file,"-percent.txt"))
}