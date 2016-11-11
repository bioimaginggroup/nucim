#' Barplot with Intervals
#'
#' @param x matrix
#' @param method method for intervals: "minmax" (default) or "quantile"
#' @param qu vector of two quantiles for method="quantile
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot_with_interval<-function(x,method="minmax",qu=c(0,1),ylim=NULL,...){
  N<-dim(x)[1]
  mi<-switch(method,
             "minmax" = apply(x,1,min),
             "quantile" = apply(x,1,quantile,qu[1])
  )
  ma<-switch(method,
             "minmax" = apply(x,1,max),
             "quantile" = apply(x,1,quantile,qu[2])
  )
  me<-switch(method,
             "minmax" = apply(x,1,mean),
             "quantile" = apply(x,1,median)
  )
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
#' @param method method for intervals: "minmax" (default) or "quantile"
#' @param qu vector of two quantiles for method="quantile
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot_with_interval_23<-function(x,l,method="minmax",qu=c(0,1),ylim=NULL,...){
  N<-dim(x)[1]
  mi<-switch(method,
             "minmax" = apply(x,1:2,min),
             "quantile" = apply(x,1:2,quantile,qu[1])
  )
  ma<-switch(method,
             "minmax" = apply(x,1:2,max),
             "quantile" = apply(x,1:2,quantile,qu[2])
  )
  me<-switch(method,
             "minmax" = apply(x,1:2,mean),
             "quantile" = apply(x,1:2,median)
  )
  if (is.null(ylim))ylim=c(0,max(ma))
  me<-t(me)
  ma<-t(ma)
  mi<-t(mi)
  graphics::barplot(me,ylim=ylim, names.arg=1:N,beside=TRUE,,...)
  for(k in 1:3)for (j in 1:N)graphics::lines(4*rep(j,2)-3.5+k,c(mi[k,j],ma[k,j]),lwd=1)
  for(k in 1:3)for (j in 1:N)graphics::lines(4*(j)-3.5+k+c(-.2,.2),rep(mi[k,j],2))
  for(k in 1:3)for (j in 1:N)graphics::lines(4*(j)-3.5+k+c(-.2,.2),rep(ma[k,j],2))
}