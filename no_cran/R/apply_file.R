apply.file<-function(file,func,inputfolder="",outputfolder="",input="TIF",output="TIF",...)
{
  test<-try({
      input<-bioimagetools::readTIF(paste0(inputfolder,"/",file))
      result<-as.function(func)(input)
      bioimagetools::writeTIF(result,paste(outputfolder,"/",file,sep=""))
      gc(verbose=FALSE)
    },silent=TRUE)
    if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
    else(cat(paste0(file," OK\n")))
  
}