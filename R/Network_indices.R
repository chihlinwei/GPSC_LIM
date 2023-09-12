rm(list = ls())
library(LIM)
library(NetIndices)
#GC####
# Load your .Rdata file and view the iteration samples.
load("GC1_10000_50.Rdata")
GC_xs<-xs
GC_LIM<-LIM
# # of iteration= # of matrices #in our case=10000

# Decide which indices you want to test.
#T.. ??? Total system throughput
#TST ??? Total system throughflow
#Ltot ??? Number of links
#C ??? Connectance
#Tij ??? Average link weight

# Refer to documentation if necessary.
Indices <- c("T..", "TST", 
             "TSTC","FCI",
             "AMI")
GC_LIM$Unknowns

# Create a matrix for the indices to be calculated for each iteration
NetInd_GC <- matrix(NA,nrow=nrow(GC_xs$X),ncol=length(Indices)) ##dimnames
colnames(NetInd_GC) <- Indices
#create loop 

fm_GC  <- Flowmatrix(GC_LIM, GC_xs$X[1,])
#define: Import, Export, Dead
#Some of the indices only concern the organisms
#need to tell the computer which compartments can be used for the calculations

for (i in 1:nrow(GC_xs$X)){
  # Specify which compartments are external to serve for import and export.
  # Specify which compartments comprise of dead material.
  Import <- "POC_W"
  Export <- c("EXP","BUR","EXP_B","DIC_W") 
  Dead <- "SED"
  # Get flow matrix
  fm_GC  <- Flowmatrix(GC_LIM, GC_xs$X[i,])
  
  # Remove rows/cols that sum to zero,
  # and remove Imports from columns and Exports from rows.
  RemCol <- unique(which(colSums(fm_GC)==0),
                   which(colnames(fm_GC)%in% Import, arr.ind = TRUE))
  RemRow <- unique(which(rowSums(fm_GC)==0),
                   which(rownames(fm_GC)%in% Export, arr.ind = TRUE))
  if(length(RemRow) != 0) {fm_GC <- fm_GC[-RemRow,] }
  if(length(RemCol) != 0) {fm_GC <- fm_GC[,-RemCol] }
  
  # Redefine Import and Export if they were eliminated from fm
  Import <- Import[which(Import%in%rownames(fm_GC))]
  Export <- Export[which(Export%in%colnames(fm_GC))]      
  
  # Calculate indices
  NetInd_GC[i, "T.."] <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$T..
  NetInd_GC[i, "TST"] <- GenInd (Flow=fm_GC, Import=Import, Export=Export)$TST
  NetInd_GC[i, "TSTC"] <-PathInd (Flow=fm_GC, Import=Import, Export=Export)$TSTC
  NetInd_GC[i, "FCI"]  <-PathInd (Flow=fm_GC, Import=Import, Export=Export)$FCI
  NetInd_GC[i, "AMI"]  <- UncInd (Flow=fm_GC, Import=Import, Export=Export)$AMI

  }
save(NetInd_GC,file="GC1_Indices_new.Rdata")


#GS####
# Load your .Rdata file and view the iteration samples.
load("GS1_10000_50.Rdata")
GS_xs<-xs
GS_LIM<-LIM
# # of iteration= # of matrices #in our case=10000

# Create a matrix for the indices to be calculated for each iteration
NetInd_GS <- matrix(NA,nrow=nrow(GS_xs$X),ncol=length(Indices)) ##dimnames
colnames(NetInd_GS) <- Indices
#create loop 

fm_GS  <- Flowmatrix(GS_LIM, GS_xs$X[1,])
#define: Import, Export, Dead
#Some of the indices only concern the organisms
#need to tell the computer which compartments can be used for the calculations

for (i in 1:nrow(GS_xs$X)){
  # Specify which compartments are external to serve for import and export.
  # Specify which compartments comprise of dead material.
  Import <- "POC_W"
  Export <- c("EXP","BUR","EXP_B","DIC_W") 
  Dead <- "SED"
  # Get flow matrix
  fm_GS  <- Flowmatrix(GS_LIM, GS_xs$X[i,])
  
  # Remove rows/cols that sum to zero,
  # and remove Imports from columns and Exports from rows.
  RemCol <- unique(which(colSums(fm_GS)==0),
                   which(colnames(fm_GS)%in% Import, arr.ind = TRUE))
  RemRow <- unique(which(rowSums(fm_GS)==0),
                   which(rownames(fm_GS)%in% Export, arr.ind = TRUE))
  if(length(RemRow) != 0) {fm_GS <- fm_GS[-RemRow,] }
  if(length(RemCol) != 0) {fm_GS <- fm_GS[,-RemCol] }
  
  # Redefine Import and Export if they were eliminated from fm
  Import <- Import[which(Import%in%rownames(fm_GS))]
  Export <- Export[which(Export%in%colnames(fm_GS))]      
  
  # Calculate indices
  NetInd_GS[i, "T.."] <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$T..
  NetInd_GS[i, "TST"] <- GenInd (Flow=fm_GS, Import=Import, Export=Export)$TST
  NetInd_GS[i, "TSTC"] <-PathInd (Flow=fm_GS, Import=Import, Export=Export)$TSTC
  NetInd_GS[i, "FCI"]  <-PathInd (Flow=fm_GS, Import=Import, Export=Export)$FCI
  NetInd_GS[i, "AMI"]  <- UncInd (Flow=fm_GS, Import=Import, Export=Export)$AMI
  
}
save(NetInd_GS,file="GS1_Indices.Rdata")

load("GC1_Indices.Rdata")
load("GS1_Indices.Rdata")
library(dplyr)
# Calculate the mean and standard deviation of all indices
GC <- as.data.frame(NetInd_GC)
GS <- as.data.frame(NetInd_GS)
median(GC$FCI)
median(GS$FCI)
median<-data.frame(T..=c(median(GC$T..),median(GS$T..)),
                   TST=c(median(GC$TST),median(GS$TST)),
                   TSTC=c(median(GC$TSTC),median(GS$TSTC)),
                   FCI=c(median(GC$FCI),median(GS$FCI)),
                   AMI=c(median(GC$AMI),median(GS$AMI)))
# Set seed for significance calculations. 
# Using this seed 200 random values are rendered which are used in
# the significance calculations.
# (Indices of used seeds: 1 to 101)
set.seed(666)
seeds <- sample(100)
seeds
findoverlap <- function(matrix1, matrix2, seed1, seed2){
  # Randomize order of rows in both matrices, unless stated otherwise (seed1 = 0)
  if(seed1 != 0){
    set.seed(seed1)
    rand1 <- sample(nrow(matrix1))
    matrix1 <- matrix1[rand1,,drop = FALSE]
    set.seed(seed2)
    rand2 <- sample(nrow(matrix2))
    matrix2 <- matrix2[rand2,,drop = FALSE]
  }
  
  # Find fraction of rows where value in M1 > M2.
  fractions <- rep(NA, length(ncol(matrix1)))
  
  for(i in 1:ncol(matrix1)){
    fractions[i] <- length(which(matrix1[,i] > matrix2[,i]))/length(matrix1[,i])
  }
  
  return(fractions)
}
fraction <- findoverlap(NetInd_GC,NetInd_GS, seeds[30], seeds[70])
fraction

