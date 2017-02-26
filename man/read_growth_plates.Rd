\name{read_growth_plates}
\alias{read_growth_plates}

\title{Import plate reader results for multiple replicate plates.}

\description{This function reads a list of files from the plate reader results and combines all the results into one file, and returns a single data frame with all the information. It can take an arbitrary name of replicates, conditions and time points, as long as the same layout is used on every plate.}

\usage{
read_growth_plates(plate_map_file,well_map_file,dir,time.var="hrs")
}

\arguments{
  \item{plate_map_file}{File path of a tab delimited file containing labeled columns: "plateID","condition","rep" and "hrs". Columns condtion and rep can be any arbitrary labels. The column plateID must have unique identifiers for each plate reading that matches exactly the filenames of the plate reader results. hrs must be a numeric column indicating time in hours.}
  \item{well_map_file}{Tab delimited file that specifies the plate layout. Columns must be labelled numerically and rows alphabetically.}
  \item{dir}{Directory that contains all the individual plate readings. All the files from the plate reader must be placed directly under this directory and they must be named <plateID>.txt, where <plateID> matches entries in the plateID column of plate_map_file.}
  \item{time.var}{Not fully implemented yet. Must eventually allow for arbitrary name for time variable. Do not modify.}
}

\details{Non yet}

\value{Returns a data frame where each row represents an observation with the following columns:
  \item{Row}{Row label of the observation.}
  \item{Colum}{Column label of the observation.}
  \item{OD600}{Plate reader reading for the given observation.}
  \item{rep}{Replicate label of the observation.}
  \item{hrs}{Time point at which observation was made.}
  \item{condition}{condition label of the observation.}
  \item{Strain}{Label of the strain in the give well (from well_map_file).}
}

\author{Sur from Dangl Lab}

\examples{
## NEED DATA BEFORE ADDING EXAMPLE!!
}