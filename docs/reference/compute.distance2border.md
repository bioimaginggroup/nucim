# Compute distance to border of classes

Compute distance to border of classes

## Usage

``` r
compute.distance2border(
  f,
  color,
  N,
  from.spots = FALSE,
  output = "dist2border",
  cores = 1
)
```

## Arguments

- f:

  folder of classes images

- color:

  folder of color images ("spots-"color for spots images)

- N:

  which class

- from.spots:

  Logical.

- output:

  output folder

- cores:

  number of parallel cores which can be used

## Value

images in output"-"color"-"N
