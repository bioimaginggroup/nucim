#' Plots all distances to next neighbour of all classes for folders
#'
#' @param path path to folder 
#' @param N number of classes, default: 7
#' @param cores number of cores to use in parallel (needs parallel package if cores>1)
#' @param method method for summarising distances, either "min" or "quantile"
#' @param qu quantile for method="quantile", default: 0.01
#' @export
#' @return plots
#' 
plot_nearestClassDistances.folder<-function(path, N=7, cores=1, method="quantile", qu=0.01)
{
  orig<-getwd()
  setwd(path)
  
  files<-list.files("distances")
  cat(paste(length(files),"files.\n"))
  
  if (cores>1)
  {
    ff<-length(files)
    cores1=min(floor(sqrt(cores)),ff)
    cores2=floor(cores/cores1)
  }
  
  cat("Prepare plot")
  if(cores>1)dist <- parallel::mclapply(files, ncd.helper, method=method, qu=qu, cores=cores2, mc.preschedule=FALSE, mc.cores=cores1)
  if(cores==1)dist <- lapply(files, ncd.helper, method=method, qu=qu)
  
  dist<-array(unlist(dist),c(N,N,length(files)))
  
  mfrow=ceiling(N^(2/3))
  mfrow=c(ceiling(N/mfrow),mfrow)
  graphics::par(mfrow=mfrow)
  m<-apply(dist,1:2,mean)+0.5*apply(dist,1:2,sd)
  ylim=ceiling(c(0,max(m))/0.05)*0.05
  for (i in 1:N)
  {
    border=rep(NA,N)
    border[N+1-i]="red"
    col=rep("grey",N)
    col[N+1-i]="white"
    barplot_with_interval(dist[i,,],xlab="",horiz=TRUE,method="sd",border=border, col=col,ylim=ylim)
  }
  setwd(orig)
  cat("\n")
}
ncd.helper<-function(file, method="quantile", qu=0, cores=1)
{
  try({
    load(paste0("distances/",file))
  N<-length(distances)
  if (cores>1)if (method=="quantile")distances<-parallel::mclapply(distances,ncd.helper.qu,qu=qu, mc.cores=cores)
  if (cores==1)if (method=="quantile")distances<-lapply(distances,ncd.helper.qu,qu=qu)
  if (cores>1)if (method=="min")distances<-parallel::mclapply(distances,ncd.helper.min, mc.cores=cores)
  if (cores==1)if (method=="min")distances<-lapply(distances,ncd.helper.min)
  
  distances<-array(unlist(distances),c(N,N))
  return(distances)
  })
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

  