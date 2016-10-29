if (0)
  {
  setwd("~/bioimg/projects/marion/YM")
library(EBImage)
library(bioimagetools)
library(parallel)
library(tiff)
min.spots<-400
N<-7
#N<-5
color="green"
color="red"
for (N in c(5,7))
  for (color in c("red","green"))
    {   

    files<-sample(list.files("blue"))
    dir<-paste("dist2border-",color,"-",N,sep="")
    if(length(list.files("dir"))==0)dir.create(dir)
    
    #foreach (file = files) %dopar%
    for (file in files)
#    do<-function(file)
        
    {
    try({
      print(file)
      img<-scan(paste("XYZmic/",file,".txt",sep=""))
      Xmic<-img[1]
      Ymic<-img[2]
      Zmic<-img[3]
      remove(img)
      
      col<-readTIF(paste(color,"/",file,sep=""))
      class<-readTIF(paste("class",N,"/",file,sep=""))
      mask<-readTIF(paste("dapimask/",file,sep=""))
      
      X<-dim(col)[1]
      Y<-dim(col)[2]
      Z<-dim(col)[3]
      
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
      
      nn<-min.spots
      n.spots=0
      n.sp=0
      l.col2<-length(col2)
      while(n.sp<min.spots)
      {
      thresh<-quantile(col2,1-nn/l.col2)  
      white<-array(ifelse(col>thresh,1,0)*mask,dim(col))
      spots1<-bwlabel3d(white)
      if(n.sp>max(spots1))break
      spots=spots1
      n.sp=max(spots)
      nn<-1.05*nn*min.spots/n.sp
      print(c(nn,n.sp))
      }
      
      points<-cmoments3d(spots,col)
      
      remove(col)
      remove(col2)
      points<-points[,-c(1,5)]
      colnames(points)<-c("X","Y","Z")
      points[,1]<-(points[,1]-1)/X*Xmic
      points[,2]<-(points[,2]-1)/Y*Ymic
      points[,3]<-(points[,3]-1)/Z*Zmic
                     
      d2b = distance2border(points,class,Xmic,Ymic,Zmic,class1=1,mask=mask,hist=TRUE)
      save(d2b,file=paste(dir,"/",file,"_",min.spots,".Rdata",sep=""))
    })
    }
    #jobs <- mclapply(files, function(x) mcparallel(do(x)))
    
  }
}