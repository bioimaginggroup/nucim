\name{nucim}
\alias{split.channels}
\alias{classify}
\title{Function for analysis of nucleus images}
\description{
}
\usage{
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\details{
}
\value{
}
\author{
Volker Schmid
}
\note{
}
\examples{
if(0){
library(nucim)
f<-"~/projects/marion/Sep17"
N<-7
split.channels(f)
dapimask(f)
classify(f,N)
find.spots(f,"red")
find.spots(f,"green")
compute.distance2border(f,"green",N)
compute.distance2border(f,"red",N)
distance2border.report(f,N)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{do}
