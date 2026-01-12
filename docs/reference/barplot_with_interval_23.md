# Barplot with Intervals for two or three bars beside

Barplot with Intervals for two or three bars beside

## Usage

``` r
barplot_with_interval_23(x, method = "minmax", qu = c(0, 1), ylim = NULL, ...)
```

## Arguments

- x:

  array

- method:

  method for intervals: "minmax" (default), "quantile" or "sd"

- qu:

  vector of two quantiles for method="quantile

- ylim:

  limits for y axis. Default:NULL is ylim=c(0,max(interval))

- ...:

  additional parameters forwarded to barplot

## Value

plot
