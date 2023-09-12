rm(list=ls())
#-- Load the LIM (and limSolve) package 
library(LIM)
library(splus2R)
#MODEL SETUP####
#-- Define directory that contains the input file
DataDir <- "C:/Users/sarah/Downloads/LIM_GPSC_P/for_pub/RMD/"
#-- Read the ascii files
File<- paste(DataDir,"GS1.input",sep="")  
LIM<- Setup(file=File) 
#Parsimonious####
# Find the solution range of each flow
flowSol <- Xranges(LIM)
# Find the parsimonious solution of each flow
pars <- Lsei(LIM, parsimonious = TRUE)
# Print your ranges and parsimonious solution on screen
SSA<-data.frame(flowSol, parsimonious=pars$X)
# plot parsimonious result
plotweb(Flowmatrix(LIM),main="GS1",
        sub="(mgC/m2/d)",val=F, val.size = 0.6,
        lab.size=0.8)

Flowmatrix(LIM)
#Likelihood####
#-- SSA is not based on ecological theory
xranges<-Xranges(LIM)
x0 <- lsei(E=LIM$A,
           F=LIM$B,
           A=diag(LIM$NUnknowns),
           B=rowMeans(xranges),
           G=LIM$G,
           H=LIM$H)$X
iter<-10000
jumpsize<-50
xs <- xsample(E    = LIM$A,
              F    = LIM$B,
              G    = LIM$G,
              H    = LIM$H,
              jmp  = (xranges[,2] - xranges[,1])/jumpsize,
              x0   = x0,
              iter = iter)
nameoutput <- "GS1_10000_50.Rdata" #1000=iteration
save(xs, LIM, file=nameoutput)

#check#### 
#1) if the number of iterations is high enough to produces convergence of mean and sd
#2) if the range of sampled values cover the range of possible solutions.
#advice: examine a couple of flows(3 or so) of different magnitudes
#flow 8: BAC->MAC (10^-2)
#flow 9: BAC->DIC_W (10^1)
#flow 15: MAC->DIC_W (10^-3)
step <- 10 #step size you want to sample on iteration
#if iteration <1000 use 1; if >1000, use iteration/1000 

load(file=nameoutput)
flownrs <- c(1:17)
nsample <- iter/step
meanvalues <- cbind(LIM$Unknowns, colMeans(xs$X))
standarddev <- cbind(LIM$Unknowns, sqrt(diag(var(xs$X))))
LA<-data.frame(flow=LIM$Unknowns, 
               mean=colMeans(xs$X),
               sd=sqrt(diag(var(xs$X))))
for(flownr in flownrs){
  means   <- vector("numeric", nsample)
  sdevs   <- vector("numeric", nsample)
  for(i in 1:nsample){
    random   <- sort(runif(iter), index.return=TRUE)
    sdevs[i] <-   sd(xs$X[random$ix[1:(i*step)], flownr])
    means[i] <- mean(xs$X[random$ix[1:(i*step)], flownr])
  }
  # need a measure of when mean and sd not significantly fluctuate anymore
  # calculate fluctuation reduction
  # Error margin of 2% of average stand. dev.
  flowname<-LA[flownr,1]
  outname<-paste("GS1","flow nr",flownr,sep="_")
  png(paste("convergence",outname,".png",sep = "_"))
  par(mfrow=c(2,1))
  MEANofflow <- as.numeric(meanvalues[flownr, 2])
  plotMEANmax<-MEANofflow+MEANofflow*0.1
  plotMEANmin<-MEANofflow-MEANofflow*0.1
  plot(means,ylim=c(plotMEANmin,plotMEANmax),
       main=paste("Convergence of Mean","GS1",flowname,sep = "_"))
  stdevofflow <- as.numeric(standarddev[flownr, 2])
  errormargin <- stdevofflow * 0.02
  plotSDmax<-stdevofflow+stdevofflow*0.1
  plotSDmin<-stdevofflow-stdevofflow*0.1
  plot(sdevs,ylim=c(plotSDmin,plotSDmax),
       main=paste("Convergence of SD","GS1",flowname,sep = "_"))
  abline(h = stdevofflow + errormargin/2)
  abline(h = stdevofflow - errormargin/2)
  dev.off()
}

#check jumpsize before increase the number of iteration 
#check jumpsize=coverage of the whole range solution
samplerange <- data.frame(xranges)
samplerange$samplemin <- apply(xs$X, 2, min)
#apply(X, margin=1(rows) or 2(column), function)
samplerange$samplemax <- apply(xs$X, 2, max)
samplerange$percCover <- ((samplerange$samplemax - samplerange$samplemin) / (samplerange$max - samplerange$min) * 100)
print(samplerange, digits = 2)

# Mean percentage covered range
mean(samplerange$percCover, na.rm = T)
#increase jumpsize= decrease jumpsize parameter
#take a longer time to run

LA$mean
#burial efficiency
BE<-LA$mean[2]/LA$mean[1]
#export efficiency
EE<-LA$mean[3]/LA$mean[1]
#calculate mean and sd of TOU and BMU 
library(dplyr)
OU<-data.frame(BAC=xs$X[,10],
               MEI=xs$X[,13],
               MAC=xs$X[,16])
OU$TOU<-OU$BAC+OU$MEI+OU$MAC
OU$BMU<-OU$MEI+OU$MAC
mean(OU$TOU)
sd(OU$TOU)
mean(OU$BMU)
sd(OU$BMU)
