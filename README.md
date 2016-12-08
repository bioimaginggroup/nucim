# nucim R package

nucim (Nucleome Imaging Toolbox) is an R package for quantitative analyses of the 3D nuclear landscape recorded with super-resolved fluorescence microscopy.

## Installation 

The stable version is available on CRAN:

    setRepositories(ind=c(1,2))
    install.packages(c("nucim"))

The developement version is available on github. The developement version probably depends on the developement version of bioimagetools:

    setRepositories(ind=c(1,2))
    install.packages(c("devtools","tiff","EBImage"))
    devtools::install_github("bioimaginggroup/bioimagetools")
    devtools::install_github("bioimaginggroup/nucim")

## Contributors

This package is developed at the BioImaging group at the Department of Statistics, in cooperation with the Biocenter, Department of Biology II, both at LMU Munich.

* Main development and implementation: Volker J Schmid
* Developement of workflow: Marion and Thomas Cremer
* Input on specific parts: Barbara Hübner, Yolanda Markaki, Jens Popken, Lothar Schermelleh, Daniel Smeets