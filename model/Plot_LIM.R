rm(list=ls())
#-- Load the LIM (and limSolve) package 
library(LIM)
library(splus2R)
#MODEL SETUP####
#-- Define directory that contains the input file
DataDir <- "C:/Users/user/Downloads/labWei/Tung_thesis/GPSC_data/"
#GC1####
load(file="GC1.Rdata")
GC1_LIM<-LIM
GC1_xs<-xs
#GS1####
load(file="GS1.Rdata")
GS1_LIM<-LIM
GS1_xs<-xs

GC1<-data.frame(flow=GC1_LIM$Unknowns,
                mean=round(colMeans(GC1_xs$X),3),
                station="GC1",
                arrow=round(log(colMeans(GC1_xs$X)),2)+6)
GS1<-data.frame(flow=GS1_LIM$Unknowns,
                mean=round(colMeans(GS1_xs$X),3),
                station="GS1",
                arrow=round(log(colMeans(GS1_xs$X)),2)+6)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
GC1
#GC1####
#slightly revise POC->SED and SED->EXP_S
GC1_graph<-grViz("
digraph{
   graph[rankdir=TB]
  
  {rank=1
    node [shape=box style=filled fillcolor=lightsteelblue]
    POC_W [label=POC]
    EXP_S [label=Burial]
    DIC_W [label=DIC]
    EXP_B [label=Predation]
  }
  
  {
    rank=2
    node [style=filled fillcolor=Coral]
    DET [label=Sediment]
    BAC [label=Bacteria]
    MEI [label=Meiofauna]
    MAC [label=Macrofauna]
    }  
  
  {
  # POM input
  POC_W -> DET [color = tomato fontcolor=steelblue3 label=131.081 penwidth=15]
  # POM output
  DET -> EXP_S [color = tomato fontcolor=steelblue3 label=115.770 penwidth=12]
  
  # Deposit feeding
  DET -> BAC   [color = black  fontcolor=steelblue3 label=24.653 penwidth=9.20]
  DET -> MEI   [color = black fontcolor=steelblue3 label=0.135  penwidth=4.00]
  DET -> MAC   [color = black  fontcolor=steelblue3 label=0.012  penwidth=1.54]
  
  #Faeces
  BAC -> DET   [color = black fontcolor=steelblue3 label=9.333 penwidth=8.23]
  MEI -> DET   [color = black fontcolor=steelblue3 label=0.143 penwidth=4.05]
  MAC -> DET   [color = black  fontcolor=steelblue3 label=0.012 penwidth=1.61]
  
  # Interaction
  # BAC
  BAC -> MEI   [color = black fontcolor=steelblue3 label=0.174 penwidth=4.25]
  BAC -> MAC   [color = black fontcolor=steelblue3 label=0.012 penwidth=1.54]

  # MEI
  MEI -> MAC   [color = black fontcolor=steelblue3 label=0.012 penwidth=1.54]
  
  # Predation
  MEI -> EXP_B   [color = gray fontcolor=steelblue3 label=0.048 penwidth=2.96]
  MAC -> EXP_B   [color = gray fontcolor=steelblue3 label=0.015 penwidth=1.77]
 
  # Respiration
  BAC -> DIC_W [color = gray fontcolor=steelblue3 label=15.134 penwidth=8.72]
  MEI -> DIC_W [color = gray fontcolor=steelblue3 label=0.107 penwidth=3.77]
  MAC -> DIC_W [color = gray fontcolor=steelblue3 label=0.008 penwidth=1.15]
}}
")
GC1_graph%>%
  export_svg %>% charToRaw %>% rsvg_png("GC1_graph.png")

#GS1####
GS1
GS1_graph<-grViz("
digraph{
   graph[rankdir=TB]
  
  {rank=1
    node [shape=box style=filled fillcolor=lightsteelblue]
    POC_W [label=POC]
    EXP_S [label=Burial]
    DIC_W [label=DIC]
    EXP_B [label=Predation]
  }
  
  {
    rank=2
    node [style=filled fillcolor=Coral]
    DET [label=Sediment]
    BAC [label=Bacteria]
    MEI [label=Meiofauna]
    MAC [label=Macrofauna]
    }  
  
  {
  # POM input
  POC_W -> DET [color = tomato fontcolor=steelblue3 label=78.946 penwidth=10.37]
  # POM output
  DET -> EXP_S [color = tomato fontcolor=steelblue3 label=63.234 penwidth=10.15]
  
  # Deposit feeding
  DET -> BAC   [color = black  fontcolor=steelblue3 label=24.508 penwidth=9.20]
  DET -> MEI   [color = black fontcolor=steelblue3 label=3.865  penwidth=7.35]
  DET -> MAC   [color = black  fontcolor=steelblue3 label=0.402  penwidth=5.09]
  
  #Faeces
  BAC -> DET   [color = black fontcolor=steelblue3 label=9.440 penwidth=8.24]
  MEI -> DET   [color = black fontcolor=steelblue3 label=3.329 penwidth=7.20]
  MAC -> DET   [color = black  fontcolor=steelblue3 label=0.294 penwidth=4.77]
  
  # Interaction
  # BAC
  BAC -> MEI   [color = black fontcolor=steelblue3 label=3.333 penwidth=7.20]
  BAC -> MAC   [color = black fontcolor=steelblue3 label=0.208 penwidth=4.43]

  # MEI
  MEI -> MAC   [color = black fontcolor=steelblue3 label=0.198 penwidth=4.38]
  
  # Predation
  MEI -> EXP_B   [color = gray fontcolor=steelblue3 label=1.159 penwidth=6.15]
  MAC -> EXP_B   [color = gray fontcolor=steelblue3 label=0.325 penwidth=4.88]
 
  # Respiration
  BAC -> DIC_W [color = gray fontcolor=steelblue3 label=11.527 penwidth=8.72]
  MEI -> DIC_W [color = gray fontcolor=steelblue3 label=2.511 penwidth= 8.44]
  MAC -> DIC_W [color = gray fontcolor=steelblue3 label=0.190 penwidth=4.34]
}}
")
GS1_graph%>%
  export_svg %>% charToRaw %>% rsvg_png("GS1_graph.png")

legend<-
"digraph{
graph[rankdir=LR]
      rank=same
    
    node[ shape = plaintext  ]
    leg1 leg2 leg3 leg4 leg5 leg6
    leg1[  style = invis ];
    leg3[  style = invis ];
    leg5[  style = invis ];
   
    leg2[ label = Abiotic_output]
    leg4[ label = Biotic_output]
    leg6[ label = Internal]
    leg1 -> leg2[color = tomato  penwidth = 4 ]
    leg3 -> leg4[color = gray penwidth = 4 ]
    leg5 -> leg6[color = black penwidth = 4 ]
    
    node [style=filled fillcolor=Coral shape=ellipse]
    box1[label=biotic]
    node [style=filled fillcolor=lightsteelblue shape= box]
    box2[label=abiotic]

}"
grViz(legend)%>%
  export_svg %>% charToRaw %>% rsvg_png("legend.png")






#other way: not figure out
rm(list = ls())
library(LIM)
library(tidyr)
#GC####
# Load your .Rdata file and view the iteration samples.
#load("GC1_MR+BAC_10000_100.Rdata")
mat<-data.frame(flow=LIM$Unknowns,
                value=flow_mean)
compartment<-c("POC_W","SED",
               "BAC", "MEI", "MAC",
               "DIC_W","EXP_S","EXP_B")
mat_GC <- matrix(0,nrow=length(compartment),ncol=length(compartment))
colnames(mat_GC) <- compartment
rownames(mat_GC) <- compartment

flow<-data.frame(xs$X)
flow_mean<-as.numeric(colMeans(flow))
flow_name<-as.character(LIM$Unknowns)
flowdf<-as.data.frame(cbind(flow_name,flow_mean))
flowdf$flow_mean<-as.numeric(flowdf$flow_mean)
flowdf$j<-gsub(".*->","",flowdf$flow_name)
flowdf$i<-gsub("->.*","",flowdf$flow_name)
#####
mat_GC["POC_W","SED"]<-flowdf$flow_mean[flowdf$i=="POC_W"]
mat_GC["SED","BAC"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="BAC"]
mat_GC["SED","MEI"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="MEI"]
mat_GC["SED","MAC"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                        flowdf$j=="MAC"]
mat_GC["SED","EXP_S"]<-flowdf$flow_mean[flowdf$i=="SED"&
                                          flowdf$j=="EXP_S"]
mat_GC["BAC","SED"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="SED"]
mat_GC["BAC","MEI"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="MEI"]
mat_GC["BAC","MAC"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                        flowdf$j=="MAC"]
mat_GC["BAC","DIC_W"]<-flowdf$flow_mean[flowdf$i=="BAC"&
                                          flowdf$j=="DIC_W"]
mat_GC["MEI","SED"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="SED"]
mat_GC["MEI","MAC"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                        flowdf$j=="MAC"]
mat_GC["MEI","DIC_W"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                          flowdf$j=="DIC_W"]
mat_GC["MEI","EXP_B"]<-flowdf$flow_mean[flowdf$i=="MEI"&
                                          flowdf$j=="EXP_B"]
mat_GC["MAC","DIC_W"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                          flowdf$j=="DIC_W"]
mat_GC["MAC","SED"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                        flowdf$j=="SED"]
mat_GC["MAC","EXP_B"]<-flowdf$flow_mean[flowdf$i=="MAC"&
                                          flowdf$j=="EXP_B"]
#####
plotmat(mat_GC,
        name = colnames(mat_GC), lwd = 1, box.lwd = 2,
        cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
        arr.length = 0.5, box.type = "circle", box.prop = 1,
        shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
        relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
        self.shifty = 0, main = "GC1")
library(diagram)
example<-as.data.frame(Teasel)
typeof(Teasel)
curves <- matrix(nrow = ncol(Teasel), ncol = ncol(Teasel), 0)
curves[3, 1] <- curves[1, 6] <- -0.35
curves[4, 6] <- curves[6, 4] <- curves[5, 6] <- curves[6, 5] <- 0.08
curves[3, 6] <- 0.35
plotmat(Teasel, pos = c(3, 2, 1), curve = curves,
        name = colnames(Teasel), lwd = 1, box.lwd = 2,
        cex.txt = 0.8, box.cex = 0.8, box.size = 0.08,
        arr.length = 0.5, box.type = "circle", box.prop = 1,
        shadow.size = 0.01, self.cex = 0.6, my = -0.075, mx = -0.01,
        relsize = 0.9, self.shiftx = c(0, 0, 0.125, -0.12, 0.125, 0),
        self.shifty = 0, main = "Teasel population model")


