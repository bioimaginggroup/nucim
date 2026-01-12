# Split channels into files and extracts size in microns

Split channels into files and extracts size in microns

## Usage

``` r
splitchannels.file(file, channels, rgb.folder, normalize = FALSE)
```

## Arguments

- file:

  file name

- channels:

  e.g. c("red","green","blue")

- rgb.folder:

  folder with file

- normalize:

  boolean. Should we try to do normalization?

## Value

files in "./red/", "./green", "./blue" and "./XYZmic"
