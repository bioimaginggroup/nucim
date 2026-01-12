# Compute colors in classes distribution

Compute colors in classes distribution

## Usage

``` r
colors.in.classes(
  classes,
  color1,
  color2 = NULL,
  mask = array(TRUE, dim(classes)),
  N = max(classes, na.rm = TRUE),
  type = "tresh",
  thresh1 = NULL,
  thresh2 = NULL,
  sd1 = 2,
  sd2 = 2,
  col1 = "green",
  col2 = "red",
  test = FALSE,
  plot = TRUE,
  beside = TRUE,
  ylim = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- classes:

  Image of classes

- color1:

  Image of first color

- color2:

  Image of second color

- mask:

  Image mask

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

- test:

  Compute tests: "Wilcoxon" for Wilcoxon rank-sum (Mann-Whitney U),
  chisq for Chi-squared test

- plot:

  Plot barplots

- beside:

  a logical value. If FALSE, the columns of height are portrayed as
  stacked bars, and if TRUE the columns are portrayed as juxtaposed
  bars.

- ylim:

  limits for the y axis (plot)

- verbose:

  verbose mode

- ...:

  additional plotting parameters

## Value

Table of classes with color 1 (and 2)

## Details

Type of spot definitions: "thresh" or "t": Threshold based (threshold
can be given by thresh1/2 or automatically derived) "voxel" or "v":
Spots are given as binary voxel mask "intensity" or "i": Voxels are
weighted with voxel intensity. Intensity is scaled to \[0,1\] after
subtracting thresh1/2 (or automatic threshold)
