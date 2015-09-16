.onAttach<-function(libname, pkgname)
{
  requireNamespace("bioimagetools")
  requireNamespace("EBImage")
  
  packageStartupMessage(paste0("Nucim", utils::packageVersion("nucim")))
}
