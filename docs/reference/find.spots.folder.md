# Detects spots

Detects spots

## Usage

``` r
find.spots.folder(
  f,
  color,
  thresh = 1,
  thresh.auto = TRUE,
  filter = NULL,
  cores = 1
)
```

## Arguments

- f:

  path to folder

- color:

  which color, images have to be in folder with color name

- thresh:

  threshold

- thresh.auto:

  Logical. Find threshold automatically?

- filter:

  2d-filter to use before spot detection

- cores:

  number of cores to use in parallel (with parallel package only)

## Value

spot images in spot-color/, number of spots as txt files in spot-color/
