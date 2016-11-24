#' Barplot with Intervals
#'
#' @param x matrix
#' @param method method for intervals: "minmax" (default), "quantile" or "sd"
#' @param qu vector of two quantiles for method="quantile
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param horiz boolean: horizontal bars?
#' @param border border parameter forwarded to barplot, default: NA is nor border
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot_with_interval<-function(x,method="minmax",qu=c(0,1),ylim=NULL,horiz=FALSE,border=NA,...){
  N<-dim(x)[1]
  me<-switch(method,
             "minmax" = apply(x,1,mean),
             "quantile" = apply(x,1,median),
             "sd" = apply(x,1,mean)
  )
  mi<-switch(method,
             "minmax" = apply(x,1,min),
             "quantile" = apply(x,1,quantile,qu[1]),
             "sd" = me-apply(x,1,sd)
  )
  ma<-switch(method,
             "minmax" = apply(x,1,max),
             "quantile" = apply(x,1,quantile,qu[2]),
             "sd" = me+apply(x,1,sd)
  )
  if (is.null(ylim))ylim=c(0,max(ma))
  if(!horiz)
    {
    graphics::barplot(me,ylim=ylim,width=0.8,space=0.25,names.arg=1:N,...)
    for (j in 1:N)graphics::lines(rep(j-0.4,2),c(mi[j],ma[j]),lwd=1)
    for (j in 1:N)graphics::lines((j-0.4)+c(-.2,.2),rep(mi[j],2))
    for (j in 1:N)graphics::lines((j-0.4)+c(-.2,.2),rep(ma[j],2))
  }
  if(horiz){
    graphics::barplot(rev(me),xlim=ylim,space=.5,border=border,names.arg=N:1,horiz=TRUE,las=1,...)
    for (j in 1:N)graphics::lines(c(mi[j],ma[j]),rep(N*1.5-1.5*j+1,2),lwd=1)
    for (j in 1:N)graphics::lines(rep(mi[j],2),(N*1.5-1.5*j+1)+c(-.2,.2),lwd=1)
    for (j in 1:N)graphics::lines(rep(ma[j],2),(N*1.5-1.5*j+1)+c(-.2,.2),lwd=1)
  }
}

#' Barplot with Intervals for two or three bars beside
#'
#' @param x array
#' @param l number of bars beside (second dimension of x)
#' @param method method for intervals: "minmax" (default), "quantile" or "sd"
#' @param qu vector of two quantiles for method="quantile
#' @param ylim limits for y axis. Default:NULL is ylim=c(0,max(interval))
#' @param ... additional parameters forwarded to barplot
#'
#' @return plot
#' @export
#'
barplot_with_interval_23<-function(x,l,method="minmax",qu=c(0,1),ylim=NULL,...){
  N<-dim(x)[1]
  me<-switch(method,
             "minmax" = apply(x,1:2,mean),
             "sd" = apply(x,1:2,mean),
             "quantile" = apply(x,1:2,median)
  )
  mi<-switch(method,
             "minmax" = apply(x,1:2,min),
             "sd" = me - apply(x,1:2,sd),
             "quantile" = apply(x,1:2,quantile,qu[1])
  )
  ma<-switch(method,
             "minmax" = apply(x,1:2,max),
             "sd" = me + apply(x,1:2,sd),
             "quantile" = apply(x,1:2,quantile,qu[2])
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