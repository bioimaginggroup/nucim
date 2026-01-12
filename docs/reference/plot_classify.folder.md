# Plot barplot for classified images in a folder

Plot barplot for classified images in a folder

## Usage

``` r
plot_classify.folder(
  path,
  N = 7,
  cores = 1,
  col = grDevices::grey(0.7),
  method = "sd"
)
```

## Arguments

- path:

  path to folder

- N:

  number of classes, default: 7

- cores:

  number of cores to use in parallel (needs parallel package if
  cores\>1)

- col:

  color of bars, either one or a vector of hex RGB characters

- method:

  method for error bars ("sd", "minmax", "quartile")

## Value

plots
