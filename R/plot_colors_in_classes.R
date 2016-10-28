#' Plot for colors in classes distribution for folders
#'
#' @param path 
#' @param col1 
#' @param col2 
#'
#' @return plot
#' @export
#' @import utils
#'
plot_colors.in.classes.folder<-function(path,col1="green",col2="red")
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files("colorsinclasses")
  cat(paste(length(files),"files.\n"))
 
  cic <- parallel::mclapply(paste0("colorsinclasses/",files),read.table,header=TRUE)
  
  l<-dim(cic[[1]])[2]
  N<-dim(cic[[1]])[1]
  f<-length(cic)
  cic <- array(unlist(cic),c(N,l,f))
  l<-l/2
  cic<-cic[,1:l,]
  
  if (l==2)colo=c("gray",col1)
  if (l==3)colo=c("gray",col1,col2)
  
  barplot.with.intervall.23(cic,l,method="minmax",col=colo)
    
}