.find.mode<-function(x)
{
  d<-density(x)
  return(d$x[which(d$y==max(d$y))[1]])
}

split.channel<-function(img)
{
    
    D<-length(dim(img))
    if (D==4)
    {
      red<-img[,,1,]
      green<-img[,,2,]
      blue<-img[,,3,]
    }
    if (D==3)
    {
      Z<-dim(img)[3]
      red<-img[,,seq(1,Z,by=3)]  
      green<-img[,,seq(2,Z,by=3)]  
      blue<-img[,,seq(3,Z,by=3)]  
    }
    
    red<-red-.find.mode(red)
    green<-green-.find.mode(green)
    bluecut<-blue-.find.mode(blue)
    
    red[red<0]<-0
    green[green<0]<-0
    bluecut[bluecut<0]<-0
    
    red<-red-min(red)
    green<-green-min(green)
    bluecut<-bluecut-min(bluecut)
    blue<-blue-min(blue)
    
    red<-red/max(red)
    green<-green/max(green)
    blue<-blue/max(blue)
    bluecut<-bluecut/max(bluecut)
        
    Xmic<-attr(img,"x.resolution")
    Ymic<-attr(img,"y.resolution")
    Zmic<-as.numeric(attr(img,"slices"))*as.numeric(attr(img,"spacing"))
    
    return(list("red"=red,"green"=green,"blue"=blue,"mic"=c(Xmic,Ymic,Zmic)))
}           
