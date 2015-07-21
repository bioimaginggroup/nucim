#' Detects spots  
#'
#' @param f path to folder
#' @param color which color, images have to be in folder with color name
#' @param cores number of cores to use in parallel (with parallel package only)
#'
#' @return spot images in spot-color/, number of spots as txt files in spot-color/
#' @export
#'
find.spots<-function(f,color,thresh=1,thresh.auto=TRUE,cores=1)
{
    orig<-getwd()
setwd(f)
    library(bioimagetools)
    if (cores>1)
    {
      library(parallel)
      options("mc.cores"=cores)
    }
    


    files<-sample(list.files(color))
    dir<-paste("spots-",color,sep="")
    if(length(list.files(dir))==0)dir.create(dir)
    
    if(cores>1)jobs <- mclapply(files, find.spots.file, dir=dir,color=color,thresh=thresh,thresh.auto=thresh.auto)
    if(cores==1)jobs <- lapply(files, find.spots.file, dir=dir,color=color,thresh=thresh,thresh.auto=thresh.auto)
    
    setwd(orig)
}

find.spots.file<-function(file, dir,color,thresh,thresh.auto)
    {
      try({
        print(file)
        
        col<-readTIF(paste(color,"/",file,sep=""))
        mask<-readTIF(paste("dapimask/",file,sep=""))
        
        col2<-col[mask==1]
        col2<-col2[col2!=0]
        
        if (thresh.auto){
          
        thresh.prop<-c()
        
        for (i in 100:200)
        {
          temp<-hist(col2,breaks=i,plot=FALSE)
          m<-min(which(diff(temp$counts)>0))
          thresh.prop<-c(thresh.prop,temp$mids[m])
        }
        gc()
        
        thresh.prop<-quantile(thresh.prop,na.rm=TRUE,probs=1/2)
        #thresh<-max(thresh,na.rm=TRUE,2/3)
        thresh=thresh*thresh.prop
        }
        
        white<-array(ifelse(col>thresh,1,0)*mask,dim(col))
        spots<-bwlabel3d(white)
        #spots<-aperm(spots,c(2,1,3))
        writeTIF(spots/2^16,file=paste(dir,"/",file,sep=""),bps=16L)
        write(max(spots),file=paste(dir,"/",file,".txt",sep=""))
      })
    }
    
    