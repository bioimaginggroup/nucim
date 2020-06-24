#' Plot for colors in classes distribution for folders
#'
#' @param path path to folder
#' @param col1 color of channel 1
#' @param col2 color of channel 2
#'
#' @return plot
#' @export
#'
plot_colors.in.classes.folder<-function(path,col1="green",col2="red")
{
  orig<-getwd()
  setwd(path)
  
  cic<-loadcic()
  l<-dim(cic)[2]
  if (l==2)colo=c("grey",col1)
  if (l==3)colo=c("grey",col1,col2)
  
  barplot_with_interval_23(cic, method="sd", col=colo)
  setwd(orig)
}

loadcic<-function(percentage=TRUE){
  files<-list.files("colorsinclasses")
  cat(paste(length(files),"files.\n"))
  
  cic <- parallel::mclapply(paste0("colorsinclasses/",files),utils::read.table,header=TRUE)
  
  l<-dim(cic[[1]])[2]
  N<-dim(cic[[1]])[1]
  f<-length(cic)
  cic <- array(unlist(cic),c(N,l,f))
  l<-l/2
  if (percentage)cic<-cic[,1:l,]
  if (!percentage)cic<-cic[,l+(1:l),]
  return(cic)
}