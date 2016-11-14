# nucim R package

nucim (Nucleus Imaging Toolbox) is an R package for quantitative analyses of the 3D nuclear landscape recorded with super-resolved fluorescence microscopy.

## Installation 

The stable version is available on CRAN:

    setRepositories(ind=c(1,2))
    install.packages(c("nucim"))

The developement version is available on github. The developement version probably depends on the developement version of bioimagetools:

    setRepositories(ind=c(1,2))
    install.packages(c("devtools","spatstat","tiff","EBImage"))
    devtools::install_github("bioimaginggroup/bioimagetools")
    devtools::install_github("bioimaginggroup/nucim")

## Contributors

This package is developed at the BioImaging group at the Department of Statistics, LMU Munich.