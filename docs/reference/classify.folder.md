# Classify DAPI

Classify DAPI

## Usage

``` r
classify.folder(f, N, beta = 0.1, output = paste0("class", N), cores = 1)
```

## Arguments

- f:

  folder

- N:

  number of classes

- beta:

  beta parameter used in bioimagetools::segment()

- output:

  output folder

- cores:

  number of cores used in parallel (needs parallel package)

## Value

results in "output" and "output"-n
