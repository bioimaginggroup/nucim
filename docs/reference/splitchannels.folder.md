# Split RGB images into channels and pixel size information

Split RGB images into channels and pixel size information

## Usage

``` r
splitchannels.folder(
  path,
  channels = c("red", "green", "blue"),
  rgb.folder = "rgb",
  normalize = FALSE,
  cores = 1
)
```

## Arguments

- path:

  Path to root folder

- channels:

  Vector of channels in images

- rgb.folder:

  Folder with RGB images

- normalize:

  boolean. Should we try to do normalization

- cores:

  Number of cores used in parallel, cores=1 implies no parallelization

## Value

Nothing, folders red, green, blue and XYZmic include separate channels
and pixel size information

## Examples

``` r
splitchannels.folder("./")
#> 0 files.
#> Nothing to do.
#> NULL
```
