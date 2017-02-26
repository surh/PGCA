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

#' Cluster growth curves
#' 
#' Performs hierarchical clusrering of growth curves based
#' on correlation distance. Cluster growth profiles of
#' different strains.
#' 
#' This funciton uses acast to convert an object from aggregate_data
#' into a matrix. Then uses pearson correlation to calculate a
#' dissimilarity betwenn profiles, where d_{ij} = 1 - p_{ij},
#' where d_{ij} is the distance between profiles i and j,
#' and p_{ij} is the pearson correlation between samples i and j.
#' Finaly ot performs hierarchical clustering on the dissimilarity matrix.
#' 
#' @param Data A data.frame
#' @param formula A formula for \link{acast} that will turn
#' the data.frame in Data into a matrix. This formula should define
#' the individual growth curves
#' @param value.var column namme containing growth curve values.
#' Value to be cast by \link{acast}.
#' @param method Method for hierarchical clustering to be
#' used by \link{hclust}.
#' 
#' @return An hclus object
#' 
#' @author Sur Herrera Paredes
#' 
#' @seealso \link{hclust} \link{acast}
#' 
#' @export
cluster_profiles <- function(Data,formula,value.var="OD600",
                             method="complete"){
  
  Dat <- acast(Data,formula,value.var=value.var)
  Dat.dis <- as.dist(1 - cor(t(Dat)))
  Dat.clus <- hclust(Dat.dis,method=method)  
  
  return(Dat.clus)
}
