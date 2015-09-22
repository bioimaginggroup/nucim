.onAttach<-function(libname, pkgname)
{
  requireNamespace("bioimagetools")
  requireNamespace("EBImage")
  
  packageStartupMessage(paste0("Nucim ver.", utils::packageVersion("nucim")))
}
