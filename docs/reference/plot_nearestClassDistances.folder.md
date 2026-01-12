# Plots all distances to next neighbour of all classes for folders

Plots all distances to next neighbour of all classes for folders

## Usage

``` r
plot_nearestClassDistances.folder(
  path,
  N = 7,
  cores = 1,
  method = "quantile",
  qu = 0.01
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

- method:

  method for summarizing distances, either "min" or "quantile"

- qu:

  quantile for method="quantile", default: 0.01

## Value

plots
