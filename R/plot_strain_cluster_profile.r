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

#' Plot strain cluster profile
#' 
#' Plots growth curves of cluster containing a given strain
#' at a given height
#' 
#' @param clustering Result from \link{cluster_profiles}
#' @param Data A data.frame.
#' @param h Clustering height threshold for making groups.
#' @param strain focal strain ID.
#' 
#' @return A ggplot2 plot
#' 
#' @author Sur Herrera Paredes
#' 
#' @export
plot_strain_cluster_profile <- function(clustering,Data,h,strain){
#   clustering <- All.sum.clus
#   Data <- All
#   h <- 0.5
#   strain <- "41"
  
  Clus.cut <- cutree(clustering,h=h)
  mem <- names(Clus.cut)[ Clus.cut == Clus.cut[strain] ]
  Clus <- Data[ Data$Strain %in% mem,]
  Clus.sum <- aggregate_data(Data=Clus,formula= OD600 ~ hrs + condition)
  
  lab <- NULL
  temp <- NULL
  for(i in 1:length(mem)){
    mod <- i %% 8 
    if(mod == 0){
      lab <- c(lab,paste(temp,collapse="."))
      temp <- NULL
    }
    temp <- c(temp,mem[i])
  }
  lab <- c(lab,paste(temp,collapse="."))
  Clus.sum$Cluster <- paste(lab,collapse="\n")
  
  p1 <- plot_growth_curves(Dat=Clus.sum,y="OD600",x="hrs",facet=~Cluster,group="condition",ncol=1,SE=TRUE)
  return(p1)
}
