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

#' Plot cluster profiles
#' 
#' Aggregate wells from the same cluster and plot them.
#' 
#' @param clustering An hclus object.
#' @param Data A data.frame with the columns define by \code{\link{read_growth_plates}}.
#' @param h Height to cut the clustering dedrogram. To be used by \code{\link{cutree}}.
#' 
#' @return A ggplot2 object from \code{\link{plot_growth_curves}}
#' 
#' @author Sur Herrera Paredes
#' 
#' @export
plot_cluster_profiles <- function(clustering,Data,h){
#   clustering <- All.sum.clus Data <- All h <- 0.5
  
  Clus.cut <- cutree(clustering,h=h)
  Res <- NULL
  for(group in unique(Clus.cut)){
    #   group < 1
    
    mem <- names(Clus.cut)[ Clus.cut == group ]
    Clus <- Data[ Data$Strain %in% mem,]
    Clus.sum <- aggregate_data(Data=Clus,formula= OD600 ~ hrs + condition)
    Clus.sum$Cluster <- paste(mem,collapse=".")
    Res <- rbind(Res,Clus.sum)
  }
  p1 <- plot_growth_curves(Dat=Res,y="OD600",x="hrs",facet=~Cluster,group="condition",ncol=1,SE=TRUE)
  
  return(p1)
}
