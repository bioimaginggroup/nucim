colors.classes<-function(f,N,color="red,green",cores=1)
{
  orig<-getwd()
  setwd(f)
  
  library(bioimagetools)
  if (cores>1)
  {
    library(parallel)
    options("mc.cores"=cores)
  }
  
  files<-list.files(paste0("class",N))
  cat(paste(length(files),"files.\n"))
  if(length(list.files(paste0("colors.classes",N)))==0)dir.create(paste0("colors.classes",N))
  if(length(list.files("redmask"))==0)dir.create("redmask")
  if(length(list.files("greenmask"))==0)dir.create("greenmask")
  
  if(cores>1)jobs <- mclapply(files, colors.classes.file, N=N, color=color, mc.preschedule=FALSE)
  if(cores==1)jobs <- lapply(files, colors.classes.file, N=N, color=color)
  setwd(orig)
}

colors.classes.file<-function(file,N,color)
{
  test<-try({
    mask<-readTIF(paste("dapimask/",file,sep=""))
    blau<-readTIF(paste("class",N,"/",file,sep=""))
    blau<-round(blau*N)
    storage.mode(blau)<-"integer"
    dored<-dogreen<-FALSE
    if (color=="red")dored=TRUE
    if (color=="green")dogreen=TRUE
    if (color=="red,green")dored=TRUE;dogreen=TRUE
    if (color=="green,red")dored=TRUE;dogreen=TRUE
    if(dored)
      {
      if (file.access(paste("redmask/",file,sep=""))==0)
      {
       red<-readTIF(paste("redmask/",file,sep=""))
      }
      else
      {
        red<-readTIF(paste("red/",file,sep="")) 
      }
      }
    if(dogreen){
      if (file.access(paste("greenmask/",file,sep=""))==0)
      {
        green<-readTIF(paste("greenmask/",file,sep=""))
      }
      else
      {
        green<-readTIF(paste("green/",file,sep="")) 
      }
    }
    
    if (dored&!dogreen){
      png(paste0("class",N,"-n/",file,".png"))
      cic<-colors.in.classes(blau,red,mask=mask,N=N,test=TRUE,plot=TRUE)
      dev.off()
      write(cic$col1.n,file=paste0("class",N,"-n/",file,"-red.txt"),ncolumns=N)
      write(cic$col1,file=paste0("class",N,"-n/",file,"-red-percent.txt"),ncolumns=N)
      write(cic$test1$p.value,file=paste0("class",N,"-n/",file,"-red-test.txt"),ncolumns=N)
      writeTIF(red>cic$thresh,paste0("redmask/",file))
    }
      
    if (!dored&dogreen){
      png(paste0("class",N,"-n/",file,".png"))
      cic<-colors.in.classes(blau,green,mask=mask,N=N,test=TRUE,plot=TRUE)
      dev.off()
      write(cic$col1.n,file=paste0("class",N,"-n/",file,"-green.txt"),ncolumns=N)
      write(cic$col1,file=paste0("class",N,"-n/",file,"-green-percent.txt"),ncolumns=N)
      write(cic$test1$p.value,file=paste0("class",N,"-n/",file,"-green-test.txt"),ncolumns=N)
      writeTIF(green>cic$thresh,paste0("greenmask/",file))
    }
    
    if (dored&dogreen){
      png(paste0("class",N,"-n/",file,".png"))
      cic<-colors.in.classes(blau,green,red,mask=mask,N=N,test=TRUE,plot=TRUE)
      dev.off()
      
      write(cic$col1.n,file=paste0("class",N,"-n/",file,"-green.txt"),ncolumns=N)
      write(cic$col1,file=paste0("class",N,"-n/",file,"-green-percent.txt"),ncolumns=N)
      writeTIF(green>cic$thresh,paste0("greenmask/",file))
      write(cic$col2.n,file=paste0("class",N,"-n/",file,"-red.txt"),ncolumns=N)
      write(cic$col2,file=paste0("class",N,"-n/",file,"-red-percent.txt"),ncolumns=N)
      writeTIF(green>cic$thresh[1],paste0("greenmask/",file))
      write(cic$test1$p.value,file=paste0("class",N,"-n/",file,"-green-test.txt"),ncolumns=N)
      write(cic$test2$p.value,file=paste0("class",N,"-n/",file,"-red-test.txt"),ncolumns=N)
      write(cic$test12$p.value,file=paste0("class",N,"-n/",file,"-red-green-test.txt"),ncolumns=N)
      writeTIF(red>cic$thresh[2],paste0("redmask/",file))
    }
    
  },silent=TRUE)
  if(class(test)=="try-error")cat(paste0(file,": ",attr(test,"condition"),"\n"))
  else(cat(paste0(file," OK\n")))
}
