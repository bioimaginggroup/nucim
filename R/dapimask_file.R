#' Automatic DAPI mask segmentation for files
#'
#' @param file file to read
#' @param folder with 
#' @return nothing, DAPI mask image will be saved to dapimask/
#' @export
#'
#'
dapimask.file<-function(file,folder="blue"){
  test<-try({
    blau<-readTIF(paste0(folder,"/",file))
    XYZ <- scan(paste0("XYZmic/",file,".txt"))
    xyzmic<-XYZ/dim(blau)
    xymic<-mean(xyzmic[1:2])
    mask<-dapimask(blau,xymic)
    writeTIF(mask,paste("dapimask/",file,sep=""),bps=8)
    remove(blau,mask,mbr,b2)
    gc(verbose=FALSE)
  },silent=TRUE)
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}  