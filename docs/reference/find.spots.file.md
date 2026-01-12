# Detects spots for one file

Detects spots for one file

## Usage

``` r
find.spots.file(
  file,
  dir,
  color,
  thresh = NULL,
  thresh.auto = FALSE,
  thresh.quantile = 0.9,
  filter = NULL,
  cores = 1
)
```

## Arguments

- file:

  file

- dir:

  directory for results

- color:

  which color, images have to be in folder with color name

- thresh:

  threshold

- thresh.auto:

  Logical. Find threshold automatically?

- thresh.quantile:

  numeric. use simple

- filter:

  2d-filter to use before spot detection

- cores:

  number of cores to use in parallel (with parallel package only)

## Value

spot images in spot-color/, number of spots as txt files in spot-color/
