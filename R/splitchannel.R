#' Split RGB channels
#'
#' @param img rgb image
#' @param preprocess logical. Should preprocessing be applied?
#'
#' @return list with red, green, blue channels and size in microns.
#' @export
#'
splitchannel<-function(img, preprocess=TRUE)
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
    
    if (preprocess)
      {
      red<-red-find.mode(red)
      green<-green-find.mode(green)
      blue<-blue-find.mode(blue)
    
    red[red<0]<-0
    green[green<0]<-0
    blue[blue<0]<-0
    
    red<-red-min(red)
    green<-green-min(green)
    blue<-blue-min(blue)
    
    red<-red/max(red)
    green<-green/max(green)
    blue<-blue/max(blue)
    }
    
    Xmic<-attr(img,"x.resolution")
    Ymic<-attr(img,"y.resolution")
    Zmic<-as.numeric(attr(img,"slices"))*as.numeric(attr(img,"spacing"))
    
    return(list("red"=red,"green"=green,"blue"=blue,"mic"=c(Xmic,Ymic,Zmic)))
}           
