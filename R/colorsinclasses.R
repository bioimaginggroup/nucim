#' Compute colors in classes distribution
#'
#' @param classes Image of classes
#' @param color1 Image of first color
#' @param color2 Image of second color
#' @param mask Image mask
#' @param N Maximum number of classes
#' @param type Type of spot definition, see details
#' @param thresh1 Threshold for first color image
#' @param thresh2 Threshold for second color image
#' @param sd1 For automatic threshold, that is: mean(color1)+sd1*sd(color1)
#' @param sd2 For automatic threshold of color2
#' @param col1 Name of color 1
#' @param col2 Name of color 2
#' @param test Compute tests: "Wilcoxon" for Wilcoxon rank-sum (Mann-Whitney U), chisq for Chi-squared test 
#' @param plot Plot barplots
#' @param beside a logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
#' @param ylim limits for the y axis (plot)
#' @param verbose verbose mode
#' @param ... additional plotting parameters
#' 
#' @details Type of spot definitions:
#' "thresh" or "t": Threshold based (threshold can be given by thresh1/2 or automatically derived)
#' "voxel" or "v": Spots are given as binary voxel mask
#' "intensity" or "i": Voxels are weighted with voxel intensity. Intensity is scaled to [0,1] after subtracting thresh1/2 (or automatic threshold)
#'  
#' @return Table of classes with color 1 (and 2)
#' @export
#' @import stats bioimagetools
#' @importFrom graphics barplot
colors.in.classes<-function(classes,color1,color2=NULL,mask=array(TRUE,dim(classes)),N=max(classes,na.rm=TRUE),type="tresh",thresh1=NULL,thresh2=NULL,sd1=2,sd2=2,col1="green",col2="red",test=FALSE,plot=TRUE,beside=TRUE,ylim=NULL,verbose=FALSE,...)
{
  no2<-ifelse(is.null(color2),TRUE,FALSE)
  classes<-array(classes,dim(classes))
  color1<-array(color1,dim(color1))
  if(!no2)color2<-array(color2,dim(color2))
  mask<-array(mask==1,dim(mask))
  classes<-classes[mask]
  color1<-color1[mask]
  if(!no2)color2<-color2[mask]
  
  if (type=="t")type="thresh"
  if (type=="v")type="voxel"
  if (type=="i")type="intensity"
  
  if(type=="thresh")
  {
  
    if(is.null(thresh1))thresh1<-mean(color1)+sd1*sd(color1)
    if(!no2)if(is.null(thresh2))thresh2<-mean(color2)+sd2*sd(color2)

    color1<-color1>thresh1
    if (!no2) color2<-color2>thresh2
  }
  
  if (type=="voxel")
  {
    color1 <- color1==1
    if (!no2) color2<-color2==1
  }
  
  weight <- weight2 <- NULL
  if (type=="intensity"|type=="ti")
  {

    if(is.null(thresh1))thresh1<-mean(color1)+sd1*sd(color1)
    if(!no2)if(is.null(thresh2))thresh2<-mean(color2)+sd2*sd(color2)

    if (verbose)cat(paste("Threshold 1 is", thresh1, "; Threshold 2 is", thresh2, "\n"))
    weight<-color1
    color1<-color1>ifelse(is.null(thresh1),0,thresh1)
    weight<-weight[color1]
    weight<-weight-min(weight)
    weight<-weight/max(weight)
    
    if (!no2){
      weight2<-color2
      color2<-color2>ifelse(is.null(thresh2),0,thresh2)
      weight2<-weight2[color2]
      weight2<-weight2-min(weight2)
      weight2<-weight2/max(weight2)
    }
  }
  
  t10<-bioimagetools::table.n(classes,m=N,percentage = FALSE)
  t1<-t10/sum(t10)
  
  t20<-bioimagetools::table.n(classes[color1],m=N, percentage = FALSE, weight=weight)
  t2<-t20/sum(t20)
  
  t3<-t30<-0
  if(!no2){
    t30<-bioimagetools::table.n(classes[color2],m=N, percentage = FALSE, weight=weight2)
    t3<-t30/sum(t30)
  }
  
  if(plot){
    tt<-rbind(t1,t2)
    if (!no2)tt<-rbind(tt,t3)
    colo<-c("grey",col1)
    if (!no2)colo<-c(colo,col2)
    if (is.null(ylim))ylim=c(0,max(c(t1,t2,t3)))
  barplot(tt,ylim=ylim,beside=beside,col=colo,names.arg = 1:N,...)
  }
  
  if(type=="intensity")
  {
    th1<-ifelse(is.null(thresh1),0,thresh1)
    if(!no2)th2<-ifelse(is.null(thresh2),0,thresh2)
    # correct for weighting
    t200=round(t20*(1-th1)*2)
    if(!no2)t300=round(t30*(1-th2)*2)
  }
  else
  {
    t200=t20
    if(!no2)t300=t30
  }
  
  if (test=="Wilcox"|test=="Wilcoxon"|test=="U")
  {
    if (sum(t10)==0)stop("No observations in DAPI classes")
    if (sum(t200)==0)stop("No observations in channel 1")
    ch1<-wilcox.test(rep(1:N,t10),rep(1:N,t200))
    cat("Wilcoxon rank-sum test DAPI vs. channel 1: p-value ")
    nucim.print.p(ch1$p.value,8)
    cat("\n")
    
    if (!no2)
    {
      if (sum(t300)==0)stop("No observations in channel 1")
      cat("Wilcoxon rank-sum test DAPI vs. channel 2: p-value ")
      ch2<-wilcox.test(rep(1:N,t10),rep(1:N,t300))
      nucim.print.p(ch2$p.value,8)
      cat("\n")
      cat("Wilcoxon rank-sum test channel 1 vs. channel 2: p-value ")
      ch3<-wilcox.test(rep(1:N,t200),rep(1:N,t300))
      nucim.print.p(ch3$p.value,8)
      cat("\n")
    }
  }
  if (test=="chisq")
  {
    ch1<-nucim.chisq.test(t10,t200)
    cat("Chi-squared test DAPI vs. channel 1: p-value ")
    nucim.print.p(ch1$p.value,8)
    cat("\n")
    if (!no2)
    {
      cat("Chi-squared test DAPI vs. channel 2: p-value ")
      ch2<-nucim.chisq.test(t10,t300)
      nucim.print.p(ch2$p.value,8)
      cat("\n")
      cat("Chi-squared test channel 1 vs. channel 2: p-value ")
      ch3<-nucim.chisq.test(t200,t300)
      nucim.print.p(ch3$p.value,8)
      cat("\n")
    }
  }
  
  ret1<-list()
  ret1[["dapi"]]<-t1
  ret1[["col1"]]<-t2
  if(!no2)ret1[["col2"]]<-t3
  ret1[["dapi.n"]]<-t10
  ret1[["col1.n"]]<-t20
  if(!no2)ret1[["col2.n"]]<-t30
  if (type=="thresh")
    {
    ret<-thresh1
    if(!no2)ret<-c(thresh1,thresh2)
    ret1[["thresh"]]<-ret
  }

  if(is.character(test))
  {
    ret1[["test1"]]<-ch1
    if (!no2)
    {
      ret1[["test2"]]<-ch2
      ret1[["test12"]]<-ch3
    }
  }
  return(ret1)
}

nucim.print.p<-function(p,n)
{
  p=round(p,n)
  if (p==0){
    cat("< 5e-0")
    cat(n+1)
  }
  else
  {
    cat("= ")
    cat(p)      
  }
}
nucim.chisq.test<-function(t1,t2)
{
  matrix<-rbind(t1,t2)+10^{-5}
  ch<-suppressWarnings(chisq.test(matrix))
  return(ch)
}
#' Compute colors in classes distribution for folders
#'
#' @param path Path to root folder
#' @param color1 Image of first color
#' @param color2 Image of second color
#' @param N Maximum number of classes
#' @param type Type of spot definition, see details
#' @param thresh1 Threshold for first color image
#' @param thresh2 Threshold for second color image
#' @param sd1 For automatic threshold, that is: mean(color1)+sd1*sd(color1)
#' @param sd2 For automatic threshold of color2
#' @param col1 Name of color 1
#' @param col2 Name of color 2
#' @param cores Number of cores used in parallel, cores=1 implies no parallelization
#' @param verbose verbose mode
#' @return Results are in folder colorsinclasses
#' @export
#' 
colors.in.classes.folder<-function(path, color1, color2=NULL, N=7, type="intensity", thresh1=NULL, thresh2=NULL, sd1=2, sd2=2, col1="green", col2="red", cores=1, verbose=FALSE)
{
  if (verbose)cores=1
  
  orig<-getwd()
  setwd(path)
  
  if(length(list.files("colorsinclasses"))==0)dir.create("colorsinclasses")
  
  files<-list.files("dapimask")
  cat(paste(length(files),"files.\n"))
  
  if (cores>1)jobs<-parallel::mclapply(files,colors.in.classes.files, color1=color1, color2=color2, type=type, thresh1=thresh1, thresh2=thresh2, sd1=sd1, sd2=sd2, col1=col1, col2=col2, mc.cores=cores)
  if (cores==1)jobs<-lapply(files,colors.in.classes.files, color1=color1, color2=color2, type=type, thresh1=thresh1, thresh2=thresh2, sd1=sd1, sd2=sd2, col1=col1, col2=col2)
  setwd(orig)
}

colors.in.classes.files<-function(file, color1, color2=NULL, N=7, type="intensity",thresh1=NULL,thresh2=NULL,sd1=2,sd2=2,col1="green",col2="red",test=FALSE, verbose=FALSE)
{
  classes<-bioimagetools::readClassTIF(paste0("class",N,"/",file))
  color1img <- bioimagetools::readTIF(paste0(color1,"/",file))
  if (!is.null(color2))color2img <- bioimagetools::readTIF(paste0(color2,"/",file))
  if (is.null(color2))color2img <- NULL
  mask <- bioimagetools::readTIF(paste0("dapimask/",file))
  cic <- colors.in.classes(classes,color1img,color2=color2img,mask=mask,N=N,type=type,thresh1=thresh1,thresh2=thresh2,sd1=sd1,sd2=sd2,col1=col1,col2=col2,test=test,plot=FALSE,verbose=verbose)

  C <- 4+2*!is.null(color2)
  cic<-data.frame(array(unlist(cic),c(N,C)))
  namescic<-c("dapi.perc","col1.perc")
  if (!is.null(color2))namescic<-c(namescic,"col2.perc")
  namescic<-c(namescic,"dapi.n","col1.n")
  if (!is.null(color2))namescic<-c(namescic,"col2.n")
  
  names(cic)<-namescic
  
  utils::write.table(cic,file=paste0("colorsinclasses/",file,".txt"),row.names = FALSE)
}
  
