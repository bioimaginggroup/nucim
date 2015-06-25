compute.distance2border<-function(f,color,N,from.spots=FALSE,cores=1)
{
  orig<-getwd()
  setwd(f)
  library(bioimagetools)
  if (cores>1)
  {
    library(parallel)
    options("mc.cores"=cores)
  }
  
 files<-sample(list.files("blue"))
 dir<-paste("dist2border-",color,"-",N,sep="")
 if(length(list.files(dir))==0)dir.create(dir)
    
 if (!from.spots)
   {
   if(cores>1) jobs <- mclapply(files, compute.distance2border.nospots.file, N, color, mc.preschedule=FALSE)
   if(cores==1) jobs <- lapply(files, compute.distance2border.nospots.file, N, color)
 }
  setwd(orig)
}

compute.distance2border.nospots.file<-function(file,N,color)
{
  test<-try({
    dir<-paste("dist2border-",color,"-",N,sep="")
    img<-scan(paste("XYZmic/",file,".txt",sep=""))
    Xmic<-img[1]
    Ymic<-img[2]
    Zmic<-img[3]
    remove(img)
    
    col<-readTIF(paste(color,"/",file,sep=""))
    class<-readTIF(paste("class",N,"/",file,sep=""))
    mask<-readTIF(paste("dapimask/",file,sep=""))
    
    col<-array(col,dim(col))
    mask<-array(mask,dim(mask))
    class<-array(round(class*N,0),dim(class))
    
    col2<-col[mask==1]
    #sdd=2
    #mcol2=mean(col2)
    #sdcol2=sd(col2)
    #thresh<-mcol2+sdd*sdcol2
    #while(sum(col2>thresh)>1000)
    #  {
    #  sdd<-sdd+1
    #  thresh<-mcol2+sdd*sdcol2
    #}
    thresh<-quantile(col2,1-1000/length(col2))
    
    points<-NULL
    X<-dim(class)[1]
    Y<-dim(class)[2]
    Z<-dim(class)[3]
    for (i in 1:X)
      for (j in 1:Y)
        for (k in 1:Z)
        {            
          if (mask[i,j,k]==1)            
          {
            if (col[i,j,k]>thresh)points<-rbind(points,c(i,j,k))
            
          }
        }
    
    remove(col)
    remove(col2)
    colnames(points)<-c("X","Y","Z")
    points[,1]<-(points[,1]-1)/X*Xmic
    points[,2]<-(points[,2]-1)/Y*Ymic
    points[,3]<-(points[,3]-1)/Z*Zmic
    
    d2b = distance2border(points,class,Xmic,Ymic,Zmic,class1=1,mask=mask,hist=TRUE)
    save(d2b,file=paste(dir,"/",file,".Rdata",sep=""))
    gc(verbose=FALSE)
  },silent=TRUE)
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}
compute.distance2border.spots.file<-function(file,dir,N,color)  
{
  try({
    dir<-paste("dist2border-",color,"-",N,sep="")
    
    img<-scan(paste("XYZmic/",file,".txt",sep=""))
    Xmic<-img[1]
    Ymic<-img[2]
    Zmic<-img[3]
    remove(img)
    
    col<-readTIF(paste(color,"/",file,sep=""))
    X<-dim(col)[1]
    Y<-dim(col)[2]
    Z<-dim(col)[3]
    
    class<-readTIF(paste("class",N,"/",file,sep=""))
    class<-array(round(class*N,0),dim(class))
    
    mask<-readTIF(paste("dapimask/",file,sep=""))
    
    spots<-readTIF(paste("spots-",color,"/",file,sep=""))
    maxspots<-scan(paste("spots-",color,"/",file,".txt",sep=""))
    spots<-array(round(spots*maxspots),dim(spots))
    
    points<-cmoments3d(spots,col)
    
    remove(col)
    remove(spots)
    gc()
    
    points<-points[,-c(1,5)]
    colnames(points)<-c("X","Y","Z")
    points[,1]<-(points[,1]-1)/X*Xmic
    points[,2]<-(points[,2]-1)/Y*Ymic
    points[,3]<-(points[,3]-1)/Z*Zmic
    
    d2b = distance2border(points,class,Xmic,Ymic,Zmic,class1=1,mask=mask,hist=TRUE)
    save(d2b,file=paste(dir,"/",file,"_",min.spots,".Rdata",sep=""))
  })
}

