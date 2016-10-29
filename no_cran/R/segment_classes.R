segment.classes<-function(img,mask,N=7)
{
    blau<-array(img,dim(img))
    blau<-round(blau*2^16)
    storage.mode(blau)<-"integer"
    img.seg<-segment(blau,N,0.1,1/3,mask=(mask==1),maxit=50,varfixed=TRUE,
                     inforce.nclust=TRUE, start="equal")
    classes<-array(as.integer(img.seg$class),dim(blau))
    return(classes)
}
