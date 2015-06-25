find.spots<-function(f,color,cores=1)
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
    
    if(cores>1)jobs <- mclapply(files, find.spots.file, dir=dir,color=color)
    if(cores==1)jobs <- lapply(files, find.spots.file, dir=dir,color=color)
    
    setwd(orig)
}

find.spots.file<-function(file, dir,color)
    {
      try({
        print(file)
        
        col<-readTIF(paste(color,"/",file,sep=""))
        mask<-readTIF(paste("dapimask/",file,sep=""))
        
        col2<-col[mask==1]
        col2<-col2[col2!=0]
        thresh<-c()
        for (i in sample(100:200,10))
        {
          temp<-hist(col2,breaks=i,plot=FALSE)
          m<-min(which(diff(temp$counts)>0))
          thresh<-c(thresh,temp$mids[m])
        }
        gc()
        
        thresh<-quantile(thresh,na.rm=TRUE,probs=2/3)
        #thresh<-max(thresh,na.rm=TRUE,2/3)
        white<-array(ifelse(col>thresh,1,0)*mask,dim(col))
        spots<-bwlabel3d(white)
        spots<-aperm(spots,c(2,1,3))
        writeTIF(spots/2^16,file=paste(dir,"/",file,sep=""),bps=16L)
        write(max(spots),file=paste(dir,"/",file,".txt",sep=""))
        
        
      })
    }
    
    