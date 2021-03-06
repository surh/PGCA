% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_growth_curves.r
\name{plot_growth_curves}
\alias{plot_growth_curves}
\title{Plot growth curves}
\usage{
plot_growth_curves(Dat, x = "hrs", y = "OD600", facet = ~Strain,
  group = "condition", ncol = 12, SE = FALSE, ymin = "OD600-OD600.se",
  ymax = "OD600+OD600.se")
}
\arguments{
\item{Dat}{A data.frame}

\item{x}{column name of time variable}

\item{y}{column name of y-axis variable (ie bacterial abundance)}

\item{facet}{faceting formula (normally a strain identifier is used)}

\item{group}{column name of variable used for grouping observations
within facet (could be different wells).}

\item{ncol}{number of columns for \link{facet_wrap}}

\item{SE}{logical, should error bars be plotted.}

\item{ymin}{if SE=TRUE, what is the lower value of the error bar.}

\item{ymax}{if SE = TRUE what is the upper value of the error bar.}
}
\value{
A ggplot2 plot
}
\description{
Plots a griwth curves on dataset. Makes a multi-panel (faceted)
plot of gorwth curves from the output of aggregate data
}
\author{
Sur Herrera Paredes
}

