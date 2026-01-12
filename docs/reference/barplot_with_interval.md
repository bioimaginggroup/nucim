# Barplot with Intervals

Barplot with Intervals

## Usage

``` r
barplot_with_interval(
  x,
  method = "minmax",
  qu = c(0, 1),
  ylim = NULL,
  horiz = FALSE,
  border = NA,
  ...
)
```

## Arguments

- x:

  matrix

- method:

  method for intervals: "minmax" (default), "quantile" or "sd"

- qu:

  vector of two quantiles for method="quantile

- ylim:

  limits for y axis. Default:NULL is ylim=c(0,max(interval))

- horiz:

  boolean: horizontal bars?

- border:

  border parameter forwarded to barplot, default: NA is nor border

- ...:

  additional parameters forwarded to barplot

## Value

plot
