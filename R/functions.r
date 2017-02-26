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

logis <- function(x,asym,xmid,scal) asym / ( 1 + exp( (xmid-x)/scal ))

wilcox_test_growth <- function(Data,compare="OD600",per="Strain",grouping="condition",ordering="hrs",
                               Comparisons=list(c("minus2plusP","plus2minusP"),
                                                c("plus2minusP","minusP"),
                                                c("minus2plusP","plusP"),
                                                c("minusP","plusP"))){
  
  Res <- matrix(nrow=length(unique(Data[,per])),ncol=length(Comparisons))
  row.names(Res) <- unique(Data[,per])
  colnames(Res) <- sapply(Comparisons,paste,collapse="_vs_")
  for(strain in unique(Data[,per])){
    Dat <- subset(Data,Data[,per] == strain)
    for(comparison in Comparisons){
      group1 <- comparison[1]
      group2 <- comparison[2]
      Dat1 <- Dat[ Dat[,grouping] == group1,]
      Dat2 <- Dat[ Dat[,grouping] == group2,]
      Dat1 <- Dat1[ order(Dat1[,ordering]),]
      Dat2 <- Dat2[ order(Dat2[,ordering]),]
      Dat.test <- wilcox.test(x=Dat1[,compare],y=Dat2[,compare],alternative="two.sided",paired=TRUE,exact=TRUE)
      Res[strain,paste(comparison,collapse="_vs_")] <- Dat.test$p.value
    }
  }
  return(Res)
}

normalize_observation <- function(x,data,variables=c(1,3),measurement=4,norm.var=2,norm.var.val=0){
  temp <- data
  for(variable in variables){
    temp <- temp[ temp[,variable] == x[variable], ]
    
  }
  substract_value <- temp[ temp[,norm.var] == norm.var.val, measurement]
  nval <- as.numeric(x[measurement]) - as.numeric(substract_value)
  #   print(temp)
  #   print(substract_value)
  return(nval)
}

read_growth_plates <- function(plate_map_file,well_map_file,dir,time.var="hrs",condition_map_file = NULL){
  pMap <- read.table(plate_map_file,header=T,colClasses="character",row.names=1)
  wMap <- read.table(well_map_file,header=T,row.names=1,sep="\t",na.strings="",colClasses="character")
  if(length(condition_map_file) > 0){
    cMap <- read.table(condition_map_file,header=T,row.names=1,sep="\t",na.strings="",colClasses="character")
    Condition <- cMap
    Condition$Row <- letters[1:nrow(Condition)]
    Condition <- melt(Condition,id.vars="Row",value.name="Condition",variable.name="Column")
  }
  
  
  Wells <- wMap
  Wells$Row <- letters[1:nrow(Wells)]
  Wells <- melt(Wells,id.vars="Row",value.name="Strain",variable.name="Column")
  
  
  All <- NULL
  for(plate in row.names(pMap)){
    # plate <- row.names(pMap)[1]
    
    plate_file <- paste(dir,"/",plate,".txt",sep="")
    Plate <- read.table(plate_file,header=T,row.names=1,sep="\t")
    Plate$Row <- letters[1:nrow(Plate)]
    Plate <- melt(Plate,id.vars="Row",value.name="OD600",variable.name="Column")
    Plate$rep <- pMap[plate,"rep"]
    Plate$hrs <- pMap[plate,"hrs"]
    
    Plate$Strain <- Wells$Strain
    if(length(condition_map_file) > 0){
      Plate$condition <- Condition$Condition
    }else{
      Plate$condition <- pMap[plate,"condition"]
    }
    
    # cat(plate,"\n")
    All <- rbind(All,Plate)
  }
  
  All[,time.var] <- as.numeric(All[,time.var])
  
  return(All)
}
