# nucim R package

nucim (Nucleome Imaging Toolbox) is an R package for quantitative analyses of the 3D nuclear landscape recorded with super-resolved fluorescence microscopy.

## Installation 

The stable version is available on [CRAN](https://cran.r-project.org/):

    setRepositories(ind=c(1,2))
    install.packages(c("nucim"))

The developement version is available on [github](https://github.com/bioimaginggroup). The developement version probably depends on the developement version of bioimagetools:

    setRepositories(ind=c(1,2))
    install.packages(c("devtools","tiff","EBImage"))
    devtools::install_github("bioimaginggroup/bioimagetools")
    devtools::install_github("bioimaginggroup/nucim")

You may need to install additional libraries on your OS before you can install nucim. E.g. on Ubuntu/Debian systems, please execute: 

    sudo apt install libssl-dev libcurl4-openssl-dev libtiff5-dev libfftw3-dev
in the terminal before installing nucim.

## More information

Description of the package and basic instructions can be found in:

* Volker J. Schmid, Marion Cremer, Thomas Cremer: Quantitative analyses of the 3D nuclear landscape recorded with super-resolved fluorescence microscopy.
Methods 123 (2017), p. 33–46. [[DOI]](https://dx.doi.org/10.1016/j.ymeth.2017.03.013)

## Contributors

This package is developed at the BioImaging group at the Department of Statistics, in cooperation with the Biocenter, Department of Biology II, both at LMU Munich.

* Main development and implementation: Volker J Schmid
* Developement of workflow: Marion and Thomas Cremer
* Input on specific parts: Barbara Hübner, Yolanda Markaki, Jens Popken, Lothar Schermelleh, Daniel Smeets

![](http://vs.lupus.uberspace.de/count/nucim.php)