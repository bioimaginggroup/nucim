# Compute colors in classes distribution for folders

Compute colors in classes distribution for folders

## Usage

``` r
colors.in.classes.folder(
  path,
  color1,
  color2 = NULL,
  N = 7,
  type = "intensity",
  thresh1 = NULL,
  thresh2 = NULL,
  sd1 = 2,
  sd2 = 2,
  col1 = "green",
  col2 = "red",
  cores = 1,
  verbose = FALSE
)
```

## Arguments

- path:

  Path to root folder

- color1:

  Image of first color

- color2:

  Image of second color

- N:

  Maximum number of classes

- type:

  Type of spot definition, see details

- thresh1:

  Threshold for first color image

- thresh2:

  Threshold for second color image

- sd1:

  For automatic threshold, that is: mean(color1)+sd1\*sd(color1)

- sd2:

  For automatic threshold of color2

- col1:

  Name of color 1

- col2:

  Name of color 2

- cores:

  Number of cores used in parallel, cores=1 implies no parallelization

- verbose:

  verbose mode

## Value

Results are in folder colorsinclasses
