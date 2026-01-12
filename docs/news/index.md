# Changelog

## nucim 1.0.13

- Minor update.

## nucim 1.0.12

- Use inherits().

## nucim 1.0.11

CRAN release: 2021-06-10

- Fixed folder change in plot_colors.in.classes.folder(),
  t_colors.in.classes.folder() and colors.in.classes.folder().
- Fixed barplot_with_interval_23() for two classes.

## nucim 1.0.9

CRAN release: 2020-05-29

- Fixed folder change in plot_classify.folder().
- Compatibility with R 4.0.0.

## nucim 1.0.8

- Workflow vignette revised.
- Removed bug from spots.combined() when using voxelsize. Thanks to
  Alexander Rapp, TU Darmstadt.

## nucim 1.0.7

- Rescale weights in colors.in.classes with intensity method. Weights
  are rescaled to \[0, 1\] for more robust results.
- Change to intensity-based weighting with automatic thresholding as
  default; old version can be used with thresh1=0, thresh2=0.

## nucim 1.0.6

CRAN release: 2018-10-09

- Add check for no observations in colors.in.classes()
- small design fixes

## nucim 1.0.0

CRAN release: 2016-12-05

- Finalized for manuscript on Quantitative analyses of the 3D nuclear
  landscape recorded with super-resolved fluorescence microscopy
  (Schmid, Cremer, Cremer)
