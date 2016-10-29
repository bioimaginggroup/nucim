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
  
  cic<-loadcic()*100
  if (test=="chisq"){
    cic<-apply(cic,1:2,sum)
    cat("Chi-squared test\n
        DAPI vs. color 1, p-value = ")
    cat(chisq.test(cic[,1],cic[,2],simulate.p.value=TRUE)$p.value)
    cat("\n")
    if (dim(cic)[2]==3)
      {
      cat("DAPI vs. color 2, p-value = ")
      cat(chisq.test(cic[,1],cic[,3],simulate.p.value=TRUE)$p.value)
      cat("\n")
      cat("color1 vs. color 2, p-value = ")
      cat(chisq.test(cic[,2],cic[,3],simulate.p.value=TRUE)$p.value)
      cat("\n")
    }
  }
  if (test=="wilcox"|test=="Wilcoxon"|test=="U"){
    cic <- round(cic)
    N <- dim(cic)[1]
    l <- dim(cic)[2]
    f <- dim(cic)[3]

    cic2 <- vector(length=l,mode="list")
    for (j in 1:l){
      for (i in 1:f){
        cic2[[j]]<-c(cic2[[j]],rep(1:N,cic[,j,i]))
      }
    }
        
    cat("Wilcoxon Rank Sum test\n
        DAPI vs. color 1, p-value = ")
    cat(wilcox.test(cic2[[1]],cic2[[2]])$p.value)
    cat("\n")
    
    if (dim(cic)[2]==3)
    {
      cat("DAPI vs. color 2, p-value = ")
      cat(wilcox.test(cic[[1]],cic[[3]])$p.value)
      cat("\n")
      cat("color1 vs. color 2, p-value = ")
      cat(wilcox.test(cic[[2]],cic[[3]])$p.value)
      cat("\n")
    }
  }
}