# (C) Copyright 2017 Sur Herrera Paredes
#
#    This file is part of PGCA.
#
#    PGCA is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    PGCA is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with PGCA.  If not, see <http://www.gnu.org/licenses/>.

#' Aggregate data
#' 
#' Simply perfromfs aggregate twice with a different function each time
#' 
#' Takes output from \code{read_growth_plates} and aggregates readings
#' from multiple replicates. Can handle an arbitrary number of replicates
#' 
#' This function is a wrapped for \code{aggregate}, see help of that
#' function for more details
#' 
#' 
#' @param Data An output from \code{read_growth_plates}, or multiple outputs
#' concatenated with \code{rbind}.
#' @param formula A formula interface indicating how to aggregate the data.
#' See help for \code{aggregate}, for more details.
#' @param FUN1 a function perform on aggregated data
#' @param FUN2 a second function to perform on aggregated data
#' 
#' @return Aggregated data.frame. For FUN2 the results will be in column
#' OD600.se
#' 
#' @author Sur Herrera Paredes
#' 
#' @export
aggregate_data <- function(Data,formula,
                           FUN1=median,FUN2=function(x){sd(x)/sqrt(length(x))}){
  Dat.sum <- aggregate(formula,data=Data,FUN=FUN1)
  Dat.sum$OD600.se <- aggregate(formula,data=Data,FUN=FUN2)$OD600
  #   Dat.sum$nOD600 <- apply(Dat.sum,1,normalize_observation,data=Dat.sum)
  return(Dat.sum)
}
