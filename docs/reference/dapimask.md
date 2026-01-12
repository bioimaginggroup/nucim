# Mask DAPI in kernel

Mask DAPI in kernel

## Usage

``` r
dapimask(
  img,
  size = NULL,
  voxelsize = NULL,
  thresh = "auto",
  silent = TRUE,
  cores = 1
)
```

## Arguments

- img:

  DAPI channel image (3d)

- size:

  size of img in microns

- voxelsize:

  size of voxel in microns

- thresh:

  threshold for intensity. Can be "auto": function will try to find
  automatic threshold

- silent:

  Keep silent?

- cores:

  number of cores available for parallel computing

## Value

mask image, array with same dimension as img.
