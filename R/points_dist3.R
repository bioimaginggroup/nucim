if(0)
  {
  setwd("~/projects/marion/Sep17")
library(bioimagetools)
library(parallel)
options("mc.cores"=6L)

N<-7
#N<-5
color="green"
color="red"
for (N in c(5,7))
  for (color in c("red","green"))
    {   

    files<-sample(list.files("blue"))
    dir<-paste("dist2border-spots-",color,"-",N,sep="")
    if(length(list.files("dir"))==0)dir.create(dir)
    
    #foreach (file = files) %dopar%
#    for (file in files)
    do<-function(file, dir)
        
    {
    try({
      print(file)
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
    jobs <- mclapply(files, do, dir, mc.preschedule=FALSE)
    
  }
}