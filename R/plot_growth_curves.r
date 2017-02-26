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

#' Plot growth curves
#' 
#' Plots a griwth curves on dataset. Makes a multi-panel (faceted)
#' plot of gorwth curves from the output of aggregate data
#' 
#' @param Dat A data.frame
#' @param x column name of time variable
#' @param y column name of y-axis variable (ie bacterial abundance)
#' @param facet faceting formula (normally a strain identifier is used)
#' @param group column name of variable used for grouping observations
#' within facet (could be different wells).
#' @param ncol number of columns for \link{facet_wrap}
#' @param SE logical, should error bars be plotted.
#' @param ymin if SE=TRUE, what is the lower value of the error bar.
#' @param ymax if SE = TRUE what is the upper value of the error bar.
#' 
#' @author Sur Herrera Paredes
#' 
#' @return A ggplot2 plot
#' 
#' @export
plot_growth_curves <- function(Dat,x="hrs",y="OD600",facet=~Strain,
                               group="condition",ncol=12,SE=FALSE,
                               ymin="OD600-OD600.se",ymax="OD600+OD600.se"){
#   Dat <- All.sum
#   x <- "hrs"
#   y <- "OD600"
#   facet <- ~ Strain
#   group <- "condition"
#   ncol <- 12
  
  p1 <- ggplot(Dat,aes_string(x=x,y=y)) +
    facet_wrap( facet ,ncol=ncol)
  
  if(SE)
    p1 <- p1 + geom_errorbar(aes_string(group=group,ymin=ymin,ymax=ymax))
  
  p1 <- p1 + geom_line(aes_string(group=group,col=group),size=1)
  
  return(p1)
}


