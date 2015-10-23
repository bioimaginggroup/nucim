#' Class neighbourhood distribution
#'
#' @param img Class image
#' @param N which class
#' @param N.max maximum class (default: 7)
#'
#' @return vector of length N.max
#' @export
#'
class.neighbours<-function(img,N, N.max=7)
{
  if (length(N)==1)return(class.neighbours.one(N, img, N.max))
  if(require(parallel))ret<-mclapply(N,class.neighbours.one, img, N.max)
  if(!require(parallel))ret<-lapply(N,class.neighbours.one, img, N.max)
  ret<-unlist(ret)
  return(t(matrix(ret,nrow=7)))
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
  ii<-img[w]
  ii<-ii[ii!=0]
  ret<-table.n(ii,N.max)
  return(ret)
}