# NEWS

Version 1.0: Finalized for manuscript on Quantitative analyses of the 3D nuclear landscape recorded with super-resolved fluorescence microscopy (Schmid, Cremer, Cremer)

Version 1.0.6: Add check for no observations in colors.in.classes(), small design fixes

Version 1.0.7: Rescale weights in colors.in.classes with intensity method. Weights are rescaled to [0, 1] for more robust results. Change to intensity-based weighting with automatic thresholding als default; old version can be implemented using thresh1=0, thresh2=0.

Version 1.0.8: Workflow vignette revised. Removed bug from spots.combined() when using voxelsize. Thanks to Alexander Rapp, TU Darmstadt.

Version 1.0.9: Fixed folder change in plot_classify.folder(). Checked for compatibility with R 4.0.0.

Version 1.0.10-11: Fixed folder change in plot_colors.in.classes.folder(), t_colors.in.classes.folder() and colors.in.classes.folder(). Fixed barplot_with_interval_23() for two classes.

Version 1.0.12: Use inherits(). 