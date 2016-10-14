#' Class neighbourhood distribution
#'
#' @param img Class image
#' @param N which class
#' @param N.max maximum class (default: 7)
#' @param cores number of cores used in parallel (needs parallel package)
#' @import parallel
#' @return vector of length N.max
#' @export
#'
class.neighbours<-function(img,N, N.max=7,cores=1)
{
  if (length(N)==1)return(class.neighbours.one(N, img, N.max))
  if(cores>1)ret<-parallel::mclapply(N,class.neighbours.one, img, N.max, mc.cores=cores)
  if(cores==1)ret<-lapply(N,class.neighbours.one, img, N.max)
  ret<-unlist(ret)
  return(t(matrix(ret,nrow=7)))
}


#' class.neighbours.folder
#'
#' @param inputfolder Input folder
#' @param outputfolder Output folder
#' @param N Max class
#' #'
#' @import bioimagetools 
#' @return plots
#' @export
#'
class.neighbours.folder<-function(inputfolder,outputfolder, N=7)
{
  if(!dir.exists(inputfolder)){print(paste(inputfolder,"not found."));return(NULL)}
    
  if(!dir.exists(outputfolder))dir.create(outputfolder)
    
  files<-list.files(inputfolder)
  for (i in files)
  {
    img<-bioimagetools::readTIF(paste(inputfolder,i,sep="/"))
    img<-round(img*7,0)
    temp<-class.neighbours(img,1:N,N.max=N)
    for (j in 1:N)
      graphics::plot(temp[j,])
  }

    
    
}

class.neighbours.one<-function(N, img, N.max=7)
{  
  w<-which(img==N, arr.ind=TRUE)
  w1<-w2<-w3<-w4<-w5<-w6<-w
  w1[,1]<-w1[,1]-1
  w2[,1]<-w2[,1]+1
  w3[,2]<-w3[,2]-1
  w4[,2]<-w4[,2]+1
  w5[,3]<-w5[,3]-1
  w6[,3]<-w6[,3]+1
  w<-rbind(w1,w2,w3,w4,w5,w6)
  w<-w[w[,1]>0,]
  w<-w[w[,2]>0,]
  w<-w[w[,3]>0,]
  w<-w[w[,1]<=dim(img)[1],]
  w<-w[w[,2]<=dim(img)[2],]
  w<-w[w[,3]<=dim(img)[3],]
  ii<-img[w]
  ii<-ii[ii!=0]
  ret<-bioimagetools::table.n(ii,N.max)
  return(ret)
}