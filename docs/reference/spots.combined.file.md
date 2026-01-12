# Find spots using information from two channels

Find spots using information from two channels

## Usage

``` r
spots.combined.file(
  file,
  size = NULL,
  voxelsize = NULL,
  folder = "./",
  thresh.offset = 0.1,
  min.sum.intensity = 2,
  max.distance = 0.5,
  use.brightest = FALSE,
  max.spots = 2,
  full.voxel = FALSE,
  output = "markers"
)
```

## Arguments

- file:

  File name

- size:

  size of img in microns, if size and voxelsize are NULL, size is
  determined from folder XYZmic

- voxelsize:

  size of voxel in microns

- folder:

  Folder

- thresh.offset:

  Thresh offset used in EBImage::thresh()

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

- output:

  output folder

## Value

RGB image with spots will be written to output folder
