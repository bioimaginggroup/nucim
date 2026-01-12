# Classify DAPI

Classify DAPI

## Usage

``` r
classify(blue, mask, N, beta = 0.1, z = 1/3, silent = TRUE)
```

## Arguments

- blue:

  DAPI channel (image)

- mask:

  mask (image)

- N:

  number of classes

- beta:

  smoothing parameter used in potts model (default: 0.1)

- z:

  scaling parameter: size of voxel in X-/Y-direction divided by the size
  of voxel in Z-direction (slice scaling parameter: size of voxel in
  X-/Y-direction divided by the size of voxel in Z-direction (slice
  thickness))

- silent:

  boolean. Should algorithm be silent?

## Value

image with classes
