# Find all distances to next neighbour of all classes for folders

Find all distances to next neighbour of all classes for folders

## Usage

``` r
nearestClassDistances.folder(
  path,
  N = 7,
  voxelsize = NULL,
  add = FALSE,
  cores = 1
)
```

## Arguments

- path:

  path to folder

- N:

  number of classes, default: 7

- voxelsize:

  real size of voxels (in microns), if NULL (default), look in folder
  XYZmic

- add:

  if TRUE, only process images which have not been processed before
  (i.e. have been added to classN)

- cores:

  number of cores to use in parallel (needs parallel package if
  cores\>1)

## Value

nothing, results are in folder distances in RData format
