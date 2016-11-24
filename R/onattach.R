.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("Nucim ", utils::packageVersion("nucim")))
}
