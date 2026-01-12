# Find spots using information from two channels

Find spots using information from two channels

## Usage

``` r
spots.combined(
  red,
  green,
  mask,
  size = NULL,
  voxelsize = NULL,
  thresh.offset = 0.1,
  window = c(5, 5),
  min.sum.intensity = 2,
  max.distance = 0.5,
  use.brightest = FALSE,
  max.spots = NA,
  full.voxel = FALSE
)
```

## Arguments

- red:

  image

- green:

  image

- mask:

  image mask

- size:

  size of img in microns

- voxelsize:

  size of voxel in microns

- thresh.offset:

  Thresh offset used in EBImage::thresh()

- window:

  Half width and height of the moving rectangular window.

- min.sum.intensity:

  spots smaller than min.sum.intensity are ignored

- max.distance:

  use only spots with distance to other color spot smaller than
  max.distance

- use.brightest:

  Logical; use only brightest in max.distance?

- max.spots:

  maximum of spots (per channel), only when use brightest=TRUE

- full.voxel:

  Logical; output contains full voxel instead of rgb intensities

## Value

RGB image with spots will be written to output folder
