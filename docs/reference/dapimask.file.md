# Automatic DAPI mask segmentation for files

Automatic DAPI mask segmentation for files

## Usage

``` r
dapimask.file(
  file,
  folder = "blue",
  voxelsize = NULL,
  size = NULL,
  silent = FALSE,
  cores = 1
)
```

## Arguments

- file:

  file to read

- folder:

  with

- voxelsize:

  real size of voxel (in microns), if NULL (default), look in folder
  XYZmic

- size:

  real size of image (in microns), if NULL (default), look in folder
  XYZmic

- silent:

  Keep silent?

- cores:

  Number of cores available for parallel computing

## Value

nothing, DAPI mask image will be saved to dapimask/
