#' Classify DAPI
#'
#' @param f folder
#' @param N number of classes
#' @param beta beta parameter used in bioimagetools::segment()
#' @param output output folder
#' @param cores number of cores used in parallel (needs parallel package)
#'
#' @return results in "output" and "output"-n
#' @export
#'
classify.folder<-function(f,N,beta=.1,output=paste0("class",N),cores=1)
{
orig<-getwd()
setwd(f)

files<-sample(list.files("blue"))
cat(paste(length(files),"files.\n"))
if(length(list.files(output))==0)dir.create(output)
if(length(list.files(paste0(output,"-n")))==0)dir.create(paste0(output,"-n"))

if(cores>1)jobs <- parallel::mclapply(files, classify.file, N=N, beta=beta, output=output, mc.preschedule=FALSE, mc.cores=cores)
if(cores==1)jobs <- lapply(files, classify.file, beta=beta, N=N, output=output)
setwd(orig)
}

classify.file<-function(file, beta, N, output)
{
test<-try({
  mask<-readTIF(paste("dapimask/",file,sep=""))
  blau<-readTIF(paste("blue/",file,sep=""))
  blau<-array(blau,dim(blau))
  blau<-round(blau*2^16)
  storage.mode(blau)<-"integer"
    img.seg<-bioimagetools::segment(blau,N,beta,1/3,mask=(mask==1),maxit=50,varfixed=TRUE,
                     inforce.nclust=TRUE, start="equal", silent=TRUE)
    classes<-array(as.integer(img.seg$class),dim(blau))
    remove(img.seg)
    gc()
    bioimagetools::writeTIF(classes/N,paste0(output,"/",file),bps=8)
  
    classes<-classes
    classes<-classes[classes!=0]
    t=table(classes)
    print(file)
    print(t)
    write(t,file=paste0(output,"-n/",file,".txt"),ncolumns=N)
    write(t/sum(t),file=paste0(output,"-n/",file,"-percent.txt"),ncolumns=N)
  
  remove(mask,blau,classes)
gc(verbose=FALSE)
},silent=TRUE)
if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
else(cat(paste0(file," OK\n")))

}

#' Classify DAPI
#'
#' @param blue DAPI channel (image)
#' @param mask mask (image)
#' @param N number of classes
#' @param beta smoothing parameter used in potts model (default: 0.1)
#' @param z scaling parameter: size of voxel in X-/Y-direction divided by the size of voxel in Z-direction (slice scaling parameter: size of voxel in X-/Y-direction divided by the size of voxel in Z-direction (slice thickness))
#' @param silent boolean. Should algorithm be silent?
#' @return image with classes
#' @export
#'

classify<-function(blue, mask, N, beta=0.1, z=1/3, silent=TRUE)
{
  blue<-array(blue,dim(blue))
  blue<-round(blue*2^16)
  storage.mode(blue)<-"integer"
  img.seg<-bioimagetools::segment(blue,N,beta,z,mask=(mask==1),maxit=50,varfixed=TRUE,
                                  inforce.nclust=TRUE, start="equal", silent=silent)
  classes<-array(as.integer(img.seg$class),dim(blue))
  return(classes)
}
#' Classify DAPI from class image
#'
#' @param class classes image
#' @param N number of classes
#'
#' @return table with number of voxels per class
#' @export
#'
classify.table<-function(class, N)
{
  class<-class[class!=0]
  t=bioimagetools::table.n(class,N)
  return(t)
}

#' Classify DAPI
#'
#' These functions are provided for compatibility with older version of
#' the nucim package.  They may eventually be completely
#' removed.
#' @param ... parameters for classify
#' @return image with classes
#' @export
#' 
classify.single<-function(...){
  .Deprecated("classify",package="nucim")
  classify(...)
}
