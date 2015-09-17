#' Title Extract spots for DHS FISH
#'
#' @param file File name
#' @param folder Folder
#' @param thresh.offset Thresh offest used in EBImage::thresh() 
#' @param min.sum.intensity spots smaller than min.sum.intensity are ignored
#' @param max.distance use only spots with distance to other color spot smaller than max.distance
#' @param use.brightest Logical; use only brightest in max.distance?
#' @param max.spots maximum of spots (per channel), only when use brightest=TRUE
#' @param full.voxels Logical; output contains full voxels instead of rgb intensities
#' @param output output folder
#'
#' @return RGB image with spots will be written to output folder
#' @export
#' @import bioimagetools
#'
extract.spots.DHS<-function(file, folder="./", thresh.offset=0.1, min.sum.intensity=2,max.distance=0.5, use.brightest=FALSE,  max.spots=2, full.voxels=FALSE, output="markers")
{
  oldwd=getwd()
  setwd(folder)
  mask<-readTIF(paste0("dapimask/",file))
  red<-readTIF(paste0("red/",file))
  red[mask==0]<-0
  green<-readTIF(paste0("green/",file))
  green[mask==0]<-0
  
  red.spots<-thresh(red,offset=thresh.offset)
  green.spots<-thresh(green,offset=thresh.offset)
  
  red.s<-bwlabel3d(red.spots)
  green.s<-bwlabel3d(green.spots)
  
  red.c<-cmoments3d(red.s,red)
  green.c<-cmoments3d(green.s,green)
  
  red.c<-red.c[red.c[,5]>min.sum.intensity,]
  green.c<-green.c[green.c[,5]>min.sum.intensity,]
  
  if (is.null(dim(red.c)))red.c<-rbind(array(red.c,c(1,length(red.c))),c(0,0,0))
  if (is.null(dim(green.c)))green.c<-rbind(array(green.c,c(1,length(green.c))),c(100,100,10))
  
  xyz<-scan(paste0("XYZmic/",file,".txt"))
  xyz<-xyz/dim(red)
  for (i in 1:dim(red.c)[1])red.c[i,2:4]<-red.c[i,2:4]*xyz
  for (i in 1:dim(green.c)[1])green.c[i,2:4]<-green.c[i,2:4]*xyz
  
  rd<-rdist(red.c[,2:4],green.c[,2:4])
  
  potential <- which(rd<max.distance,arr.ind=TRUE)
  potential.red <- potential[,1]
  potential.green <- potential[,2]
  
  if (use.brightest)
  {
    
    table(potential.red)->tt
    kill<-as.numeric(names(which(tt>1)))
    if (length(kill)>0)
    {
      weg=c()
      for (i in kill)
      {
        pog<-potential.green[potential.red==i]
        pog0<-which(green.c[pog,5]==max(green.c[pog,5]))
        for (j in pog[-pog0])
        {
          weg<-c(weg,which(potential.red==i&potential.green==j))
        }  
      }
      potential.red<-potential.red[-weg]
      potential.green<-potential.green[-weg]
    }
    
    table(potential.green)->tt
    kill<-as.numeric(names(which(tt>1)))
    if (length(kill)>0)
    {
      weg<-c()
      for (i in kill)
      {
        pog<-potential.red[potential.green==i]
        pog0<-which(red.c[pog,5]==max(red.c[pog,5]))
        for (j in pog[-pog0])
        {
          weg<-c(weg,which(potential.green==i&potential.red==j))
        }  
      }
      potential.red<-potential.red[-weg]
      potential.green<-potential.green[-weg]
    }
    
    if (length(potential.green)>max.spots)
    {
      d=c()
      for (i in 1:length(potential.green))
        d=c(d,rd[potential.red[i],potential.green[i]])
      dd<-min(d)
      dd<-min(d[-which(d==dd)])
      d<-which(d<=dd)
      potential.green<-potential.green[d]
      potential.red<-potential.red[d]
    }
  }
  else
  {
    potential.red<-unique(potential.red)
    potential.green<-unique(potential.green)
  }
  
  
  labels.red<-red.c[potential.red,1]
  labels.green<-green.c[potential.green,1]
  
  new.red<-array(0,dim(red))
  new.green<-array(0,dim(red))
  new.blue<-array(0,dim(red))
  if (!full.voxels)  for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-red[red.s==labels.red[i]]
  if (!full.voxels)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-green[green.s==labels.green[i]]
  if (full.voxels)   for (i in 1:length(labels.red))new.red[red.s==labels.red[i]]<-1
  if (full.voxels)  for (i in 1:length(labels.green))new.green[green.s==labels.green[i]]<-1
  
  blue<-readTIF(paste("blue/",file,sep=""))
  new.blue[mask==1]<-blue[mask==1]
  all<-readTIF(paste("rgb/",file,sep=""))
  all[,,1,]<-new.red
  all[,,2,]<-new.green
  all[,,3,]<-new.blue
  print(file)
  writeTIF(all,file=paste0(output,"/",file),bps=8L,reduce=TRUE)
  setwd(oldwd)
}