% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_nearest_class_folder.R
\name{plot_nearestClassDistances.folder}
\alias{plot_nearestClassDistances.folder}
\title{Plots all distances to next neighbour of all classes for folders}
\usage{
plot_nearestClassDistances.folder(
  path,
  N = 7,
  cores = 1,
  method = "quantile",
  qu = 0.01
)
}
\arguments{
\item{path}{path to folder}

\item{N}{number of classes, default: 7}

\item{cores}{number of cores to use in parallel (needs parallel package if cores>1)}

\item{method}{method for summarizing distances, either "min" or "quantile"}

\item{qu}{quantile for method="quantile", default: 0.01}
}
\value{
plots
}
\description{
Plots all distances to next neighbour of all classes for folders
}
