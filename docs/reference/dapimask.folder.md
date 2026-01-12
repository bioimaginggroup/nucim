# Automatic DAPI mask segmentation for folder

Automatic DAPI mask segmentation for folder

## Usage

``` r
dapimask.folder(
  path,
  folder = "blue",
  voxelsize = NULL,
  size = NULL,
  cores = 1
)
```

## Arguments

- path:

  path to folder with DAPI

- folder:

  folder with DAPI images

- voxelsize:

  real size of voxel (in microns), if NULL (default), look in folder
  XYZmic

- size:

  real size of image (in microns), if NULL (default), look in folder
  XYZmic

- cores:

  number of cores to use in parallel (need parallel package)

## Value

nothing, results are in folder dapimask
