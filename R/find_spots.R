#' Detects spots  
#'
#' @param f path to folder
#' @param color which color, images have to be in folder with color name
#' @param thresh threshold
#' @param thresh.auto Logical. Find threshold automatically?
#' @param filter 2d-filter to use before spot detection
#' @param cores number of cores to use in parallel (with parallel package only)
#'
#' @return spot images in spot-color/, number of spots as txt files in spot-color/
#' @export
#' @import parallel
#'
find.spots.folder<-function(f,color,thresh=1,thresh.auto=TRUE,filter=NULL,cores=1)
{
    orig<-getwd()
setwd(f)
    if (cores>1)
    {
      options("mc.cores"=cores)
    }
    


    files<-sample(list.files(color))
    dir<-paste("spots-",color,sep="")
    if(length(list.files(dir))==0)dir.create(dir)
    
    if(cores>1)jobs <- parallel::mclapply(files, find.spots.file, dir=dir,color=color,thresh=thresh,thresh.auto=thresh.auto,filter)
    if(cores==1)jobs <- lapply(files, find.spots.file, dir=dir,color=color,thresh=thresh,thresh.auto=thresh.auto,filter)
    
    setwd(orig)
}

#' Detects spotsfor one file
#'
#' @param file file
#' @param dir directory for results
#' @param color which color, images have to be in folder with color name
#' @param thresh threshold
#' @param thresh.auto Logical. Find threshold automatically?
#' @param thresh.quantile numeric. use simple 
#' @param filter 2d-filter to use before spot detection
#' @param cores number of cores to use in parallel (with parallel package only)
#'
#' @return spot images in spot-color/, number of spots as txt files in spot-color/
#' @export
#'
find.spots.file<-function(file, dir,color,thresh=NULL,thresh.auto=FALSE,thresh.quantile=.9,filter=NULL,cores=1)
{
  try({
    print(file)
    
    col<-bioimagetools::readTIF(paste(color,"/",file,sep=""))
    mask<-bioimagetools::readTIF(paste("dapimask/",file,sep=""))
    
    col2<-col[mask==1]
    col2<-col2[col2!=0]
    
    if (is.null(thresh)&!thresh.auto)thresh=quantile(col2,thresh.quantile)
    
    if (thresh.auto){
      
      thresh.prop<-c()
      
      if(cores>1)thresh.prop<-unlist(parallel::mclapply(100:200,thresh.fc,col2,mc.cores=cores))
      if(cores==1)thresh.prop<-unlist(lapply(100:200,thresh.fc,col2))
      
      thresh.prop<-quantile(thresh.prop,na.rm=TRUE,probs=1/2)
      #thresh<-max(thresh,na.rm=TRUE,2/3)
      thresh=thresh*thresh.prop
    }
    
    if (!is.null(filter))col<-filter2(col,filter)
    
    white<-array(ifelse(col>thresh,1,0)*mask,dim(col))
    spots<-bioimagetools::bwlabel3d(white)
    #spots<-aperm(spots,c(2,1,3))
    writeTIF(spots/2^16,file=paste(dir,"/",file,sep=""),bps=16L)
    write(max(spots),file=paste(dir,"/",file,".txt",sep=""))
  })
}

thresh.fc<-function(i,col2)
{
  temp<-graphics::hist(col2,breaks=i,plot=FALSE)
  m<-min(which(diff(temp$counts)>0))
  return(temp$mids[m])
}
