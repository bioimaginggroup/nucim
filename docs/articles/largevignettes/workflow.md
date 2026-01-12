# Workflow

### Workflow example

## Installation

Install *nucim* using *install.packages()*. The package relies on the
*EBImage* package, which is available on
[bioconductor](https://www.bioconductor.org), hence you need to set the
repositories accordingly:

``` r

setRepositories(ind=c(1,2))
install.packages("nucim")
```

Start with loading the necessary libraries:

``` r

library(bioimagetools)
library(nucim)
```

## Read image and mask from DAPI

In the following, we use an example dataset available online. Use the
*readTIF* function in *bioimagetools* to load the RGB image.

``` r

img = readTIF("http://ex.volkerschmid.de/cell.tif")
sections = dim(img)[4]
x = y = 0.0395
z = 0.125
blue = img[,,3,] 
```

The *dapimask* function automatically finds the kernel using the DAPI
(blue) channel. This takes about 30 seconds (all computation times are
using MacBookPro 2019, 2,4 GhZ Quad-Core i5, 16 GB RAM)

``` r

mask = dapimask(blue, c(x,y,z)*dim(img)[1:3], thresh="auto")
## 0o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.
```

## Chromatin compaction classification

Next we find compaction classes from the DAPI channel (Computation time
around 180 sec.).

``` r

classes = classify(blue, mask, 7, beta=0.1, z=x/z)
## 0o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0
```

Barplot of classes and heatmap, finally save the classes image.

``` r

tab<-table.n(classes, 7, percentage=TRUE)
barplot(tab, ylab="percentage", xlab="chromatin compaction level",col=heatmap7())
```

![](workflow_files/figure-html/plotclasses-1.png)

``` r

par(pty="s")
img(classes, z=16, col=heatmap7(), mask=mask)
```

![](workflow_files/figure-html/plotclasses-2.png)

``` r

writeTIF(classes, "classes.tif")
## [1] 52
```

## Distances of chromatin compaction classes

Find the distances between compaction classes (computation time around
480 seconds, due to parallelization this depends on number of cores).
The plotting function defines what we mean by distance: the minimum
distance between class voxels, a quantile or plot a boxplot.

``` r

classes<-readClassTIF("classes.tif")
distances = nearestClassDistances(classes,  voxelsize=c(x,y,z), classes=7, cores=4L)
save(distances,file="distances.Rdata")
plotNearestClassDistances(distances, method="min",ylim=c(0,.1),qu=.01)
plotNearestClassDistances(distances, method="quantile",ylim=c(0,.22),qu=.01)
plotNearestClassDistances(distances, method="boxplot",ylim=c(0,1.5))
```

## Map spots to classes

We can compute the distribution of other color channels on compaction
classes. Here we use a threshold approach and an intensity-weighted
approach for comparison. A test on independence of color channels and
compaction classes is automatically done (computation time around 4
sec.).

``` r

red = img[,,1,]
green = img[,,2,]
cc1<-colors.in.classes(classes,red,green,mask,7,type="thresh",plot=TRUE,
                       col1="red",col2="green",thresh1=0.05,thresh2=0.05,
                       test="Wilcoxon",ylim=c(0,.5),
                       xlab="chromatin compaction levels",ylab="percentage")
```

![](workflow_files/figure-html/cic-1.png)

    ## Wilcoxon rank-sum test DAPI vs. channel 1: p-value < 5e-09
    ## Wilcoxon rank-sum test DAPI vs. channel 2: p-value < 5e-09
    ## Wilcoxon rank-sum test channel 1 vs. channel 2: p-value = 0.00021042
    cc2<-colors.in.classes(classes,red,green,mask,7,type="intensity",plot=TRUE,
                           col1="red",col2="green",thresh1=0.05,thresh2=0.05,
                           test="Wilcoxon",ylim=c(0,.5),
                           xlab="chromatin compaction levels",ylab="percentage")

![](workflow_files/figure-html/cic-2.png)

    ## Wilcoxon rank-sum test DAPI vs. channel 1: p-value < 5e-09
    ## Wilcoxon rank-sum test DAPI vs. channel 2: p-value < 5e-09
    ## Wilcoxon rank-sum test channel 1 vs. channel 2: p-value = 0.00362231

Alternatively, we can find spots first and then use this to find the
distribution on compaction classes (computation time *spots()* around 90
sec.).

``` r

system.time({
spots<-spots.combined(red=red,green=green,mask=mask,size=c(x,y,z),
                      full.voxel=FALSE,thresh.offset=0.05)
})
## 0o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.0o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o0o.xXx.o
##    user  system elapsed 
##  39.063   7.131  46.405
```

``` r

cc<-colors.in.classes(classes,spots$red,spots$green,mask,7,type="i",plot=TRUE,
                      col1="red",col2="green",test="Wilcoxon",ylim=c(0,.8),
                      xlab="chromatin compaction levels",ylab="percentage")
```

![](workflow_files/figure-html/cicspots-1.png)

    ## Wilcoxon rank-sum test DAPI vs. channel 1: p-value < 5e-09
    ## Wilcoxon rank-sum test DAPI vs. channel 2: p-value < 5e-09
    ## Wilcoxon rank-sum test channel 1 vs. channel 2: p-value = 6.801e-05
