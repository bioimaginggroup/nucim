.tp<-function(point,classes,mask)
{
  X<-round(point[1])
  Y<-round(point[2])
  Z<-round(point[3])
#  print(c(X,Y,Z,dim(mask)))
  if(mask[X,Y,Z]==1)return(classes[X,Y,Z])
  else{return(NA)}
}

table.points<-function(classes,color,mic=FALSE,mask=array(TRUE,dim(classes)))
{
  if(!(mic==FALSE))
  {
    color[,1]<-color[,1]/mic[1]*dim(color)[1]
    color[,2]<-color[,2]/mic[2]*dim(color)[2]
    color[,3]<-color[,3]/mic[3]*dim(color)[3]
  }
  points<-apply(color,1,.tp,classes,mask)
  return(table(points[!is.na(points)]))
  
}