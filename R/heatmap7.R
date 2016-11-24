#' Heatmap colors for 7 classes
#'
#' @param ... parameters are ignored.
#' @export
#'
#' @examples barplot(7:1,col=heatmap7())
heatmap7=function(...){return(c("#3d00b2","#a200b5","#774448","#ff7b00","#ffbd00","#fff932","#ffffff"))}

#' Heatmap colors for n classes
#'
#' @param n number of colors.
#' @export
#' @importFrom fields designer.colors
#' @examples barplot(8:1,col=heatmap.color(8))
heatmap.color=function(n){
  if (n==7)return(heatmap7())
  return(color=fields::designer.colors( n=n, col= c("#3d00b2", "#ff7b00", "white")))
}
                  
