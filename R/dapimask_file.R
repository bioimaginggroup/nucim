#' Automatic DAPI mask segmentation for files
#'
#' @param file file to read
#' @param folder with 
#' @param pixelsize real pixelsize of image (in nanometers), if NULL (default), look in folder XYZmic
#' @param size real size of image (in microns), if NULL (dafault), look in folder XYZmic
#' @param silent Keep silent?
#' @param cores Number of cores available for parallel computing
#' @return nothing, DAPI mask image will be saved to dapimask/
#' @import bioimagetools
#' @export
#'
#'
dapimask.file<-function(file,folder="blue", pixelsize=NULL, size=NULL, silent=FALSE, cores=1){
  test<-try({
    blau<-readTIF(paste0(folder,"/",file))
    if (!is.null(pixelsize))size=pixelsize*dim(blau)/1000
    if (is.null(size)){
    XYZ <- scan(paste0("XYZmic/",file,".txt"))
    xyzmic<-XYZ/dim(blau)
    size<-mean(xyzmic[1:2])
    }
    mask<-dapimask(blau,size, silent=silent, cores=cores)
    writeTIF(mask,paste("dapimask/",file,sep=""),bps=8)
    remove(blau,mask)
    gc(verbose=FALSE)
  },silent=TRUE)
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}  