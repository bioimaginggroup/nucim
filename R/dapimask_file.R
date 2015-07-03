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
    mask<-dapimask(blau)
    writeTIF(mask,paste("dapimask/",file,sep=""),bps=8)
    remove(blau,mask,mbr,b2)
    gc(verbose=FALSE)
  },silent=TRUE)
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}  