#' Test for colors in classes distribution for folders
#'
#' @param path path to folder
#' @param test "Wilcoxon", "wilcox" or "U" for Wilcoxon rank-sum (Mann-Whitney U), chisq for Chi-squared test 
#'
#' @return test results
#' @export
#' @import stats
#'
t_colors.in.classes.folder<-function(path,test="Wilcoxon")
{
  orig<-getwd()
  setwd(path)
  cic<-loadcic()*100
  if (test=="chisq"){
    cic<-apply(cic,1:2,sum)
    cat("Chi-squared test\n
        DAPI vs. color 1, p-value ")
    ch1<-nucim.chisq.test(cic[,1],cic[,2])
    nucim.print.p(ch1$p.value,8)
    cat("\n")
    if (dim(cic)[2]==3)
      {
      cat("DAPI vs. color 2, p-value ")
      ch2<-nucim.chisq.test(cic[,1],cic[,3])
      nucim.print.p(ch2$p.value,8)
      cat("\n")
      cat("color1 vs. color 2, p-value = ")
      ch3<-nucim.chisq.test(cic[,2],cic[,3])
      nucim.print.p(ch3$p.value,8)
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
        DAPI vs. color 1, p-value ")
    ch1=wilcox.test(cic2[[1]],cic2[[2]])
    nucim.print.p(ch1$p.value,8)
    cat("\n")
    
    if (dim(cic)[2]==3)
    {
      cat("DAPI vs. color 2, p-value ")
      ch2=wilcox.test(cic2[[1]],cic2[[3]])
      nucim.print.p(ch2$p.value,8)
      cat("\n")
      cat("color1 vs. color 2, p-value ")
      ch3=wilcox.test(cic2[[2]],cic2[[3]])
      nucim.print.p(ch3$p.value,8)
      cat("\n")
    }
  }
  setwd(orig)
  
}