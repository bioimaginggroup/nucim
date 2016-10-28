#' Test for colors in classes distribution for folders
#'
#' @param path 
#' @param col1 
#' @param col2 
#'
#' @return plot
#' @export
#' @import stats
#'
test_colors.in.classes.folder<-function(path,test="chisq")
{
  orig<-getwd()
  setwd(path)
  
  cic<-loadcic()
  
  chisq.test(cic[,1,],cic[,2,])
  chisq.test(cic[,2,],cic[,3,])
  chisq.test(cic[,2,],cic[,3,])
  
}