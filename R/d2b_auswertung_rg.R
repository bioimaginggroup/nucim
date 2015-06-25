distance2border.report<-function(f,N,cores=1)
{
  orig<-getwd()
  setwd(f)
  library(bioimagetools)
  if (cores>1)
  {
    library(parallel)
    options("mc.cores"=cores)
  }
  

  files<-list.files("green")
  dir<-"d2bresults"
  if(length(list.files(dir))==0)dir.create(dir)
  
 if(cores>1)jobs <- mclapply(files, distance2border.report.file, N=N,mc.preschedule=FALSE)
 if(cores==1)jobs <- lapply(files, distance2border.report.file, N=N)
  
}
distance2border.report.file<-function(file,N)
{
  try({
    xlim<-c(-.3,.3)
    n<-25
    
    load(paste0("dist2border-green-",N,"/",file,".Rdata"))
    green.d<-d2b
    green.hist<-hist(d2b[d2b < xlim[2] &d2b > xlim[1]], breaks = seq(xlim[1], xlim[2], length = n), plot=FALSE )
    
    load(paste0("dist2border-red-",N,"/",file,".Rdata"))
    red.d<-d2b
    red.hist<-hist(d2b[d2b < xlim[2] & d2b > xlim[1]], breaks = seq(xlim[1], xlim[2], length = n), plot=FALSE)
    
    main = "Minimal distance to border"
    xlab = "Distance in Microns"
    
    png(paste("d2bresults/",file,"-",N,".png",sep=""))
    
    ylim<-c(0,max(c(green.hist$density,red.hist$density)))
    plot(green.hist, 
         main = paste(file,N), 
         xlab = xlab,density=10,col="green",lwd=2,freq=FALSE,ylim=ylim,ylab="",axes=FALSE)
    
    plot(red.hist, 
         xlab = xlab, angle=135,density=11,col="red",lwd=2,freq=FALSE,add=TRUE,ylim=ylim)
    
    lines(c(0,0),c(0,100),lty=2)
    axis(1)
    text(xlim[1],0.9*ylim[2],paste("green:",round(mean(green.d),3),"(",round(sd(green.d),5),")"),pos=4)
    text(xlim[1],0.8*ylim[2],paste("red:",round(mean(red.d),3),"(",round(sd(red.d),5),")"),pos=4)
    dev.off()
  })
}
