barplot.with.ci<-function(x,method="minmax",ylim=NULL,...){
  N<-dim(x)[1]
  mi<-apply(x,1,min)
  ma<-apply(x,1,max)
  me<-apply(x,1,mean)
  if (is.null(ylim))ylim=c(0,max(ma))
  graphics::barplot(me,ylim=ylim,width=0.8,space=.25,names.arg=1:N,...)
  for (j in 1:N)graphics::lines(rep(j-0.4,2),c(mi[j],ma[j]),lwd=1)
  for (j in 1:N)graphics::lines((j-0.4)+c(-.2,.2),rep(mi[j],2))
  for (j in 1:N)graphics::lines((j-0.4)+c(-.2,.2),rep(ma[j],2))
}