#' Barplot with Intervals
#'
#' @param x matrix
#' @param method method for intervals, so far only "minmax"
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot.with.interval<-function(x,method="minmax",ylim=NULL,...){
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

#' Barplot with Intervals for two or three bars beside
#'
#' @param x array
#' @param l number of bars beside (second dimension of x)
#' @param method method for intervals, so far only "minmax"
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot.with.interval.23<-function(x,l,method="minmax",ylim=NULL,...){
  N<-dim(x)[1]
  mi<-apply(x,1:2,min)
  ma<-apply(x,1:2,max)
  me<-apply(x,1:2,mean)
  if (is.null(ylim))ylim=c(0,max(ma))
  me<-t(me)
  ma<-t(ma)
  mi<-t(mi)
  graphics::barplot(me,ylim=ylim, names.arg=1:N,beside=TRUE,,...)
  for(k in 1:3)for (j in 1:N)graphics::lines(4*rep(j,2)-3.5+k,c(mi[k,j],ma[k,j]),lwd=1)
  for(k in 1:3)for (j in 1:N)graphics::lines(4*(j)-3.5+k+c(-.2,.2),rep(mi[k,j],2))
  for(k in 1:3)for (j in 1:N)graphics::lines(4*(j)-3.5+k+c(-.2,.2),rep(ma[k,j],2))
}