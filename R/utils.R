comparefilelist<-function(files,files2)
  {
  w<-unlist(lapply(files,cfl.helper,files2))
  return(files[!w])
}
  
cfl.helper<-function(file, files2)
{
  return(any(file==substr(files2,1,str_length(file))))
}