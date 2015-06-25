if(0)
  {
  setwd("~/bioimg/projects/marion/YM")
library(EBImage)
library(bioimagetools)
library(parallel)
library(tiff)
min.spots<-100
color="green"
color="red"
  for (color in c("red","green"))
    {   

    files<-sample(list.files("blue"))
    dir<-paste("spots-",color,sep="")
    if(length(list.files(dir))==0)dir.create(dir)
    
    #foreach (file = files) %dopar%
    for (file in files)
#    do<-function(file)
        
    {
    try({
      print(file)
     
      col<-readTIF(paste(color,"/",file,sep=""))
      mask<-readTIF(paste("dapimask/",file,sep=""))
      
      col2<-col[mask==1]
  
      nn<-1000
      l.col2<-length(col2)

      thresh<-quantile(col2,1-nn/l.col2)  
      white<-array(ifelse(col>thresh,1,0)*mask,dim(col))
      spots<-t(bwlabel3d(white))
      writeTIF(spots/max(spots),file=paste(dir,"/",file,sep=""))
    
    })
    }
    #jobs <- mclapply(files, function(x) mcparallel(do(x)))
    
  }
}