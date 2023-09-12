rm(list = ls())
library(LIM)
library(NetIndices)
library(dplyr)
library(tidyr)
library(vegan)
library(knitr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(grid)
library(formattable)
library(kableExtra)
load("figure_RMD/GC1_Indices.Rdata")
load("figure_RMD/GS1_Indices.Rdata")

# Calculate the mean and standard deviation of all indices
GC <- as.data.frame(NetInd_GC)
GS <- as.data.frame(NetInd_GS)

median<-data.frame(Station=c("GC1","GS1"),
                   T..=c(median(GC$T..),median(GS$T..)),
                   TST=c(median(GC$TST),median(GS$TST)),
                   TSTC=c(median(GC$TSTC),median(GS$TSTC)),
                   FCI=c(median(GC$FCI),median(GS$FCI)),
                   AMI=c(median(GC$AMI),median(GS$AMI)))
kable(median, "html",align = "c") %>% 
  kable_styling("striped", full_width = F) 
# Set seed for significance calculations. 
# Using this seed 200 random values are rendered which are used in
# the significance calculations.
# (Indices of used seeds: 1 to 101)
set.seed(666)
seeds <- sample(100)
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
df<-data.frame(Network.Index=c("T..","TST","TSTC","FCI","AMI"),
               Fraction=fraction,
               Significance=c("*** (GC1>GS1)",
                              "*** (GC1>GS1)",
                              "",
                              "*** (GC1<GS1)",
                              "*** (GC1<GS1)"))

table<- ggtexttable(df, rows = NULL,
                    theme = ttheme("blank",base_size = 13)) %>% 
  tab_add_hline(at.row = c(1, 2), row.side = "top", linewidth = 3, linetype = 1) %>% 
  tab_add_hline(at.row =6,row.side = "bottom", linewidth = 3, linetype = 1)

# long data
ind <- c("T..", "TST","TSTC","FCI","AMI")
GC_long <-
  GC %>% 
  mutate(order = 1:nrow(GC)) %>% 
  mutate(station = "GC1") %>% 
  pivot_longer(cols = all_of(ind),
               names_to = "index",
               values_to = "value")

GS_long <-
  GS %>% 
  mutate(order = 1:nrow(GS)) %>% 
  mutate(station = "GS1") %>% 
  pivot_longer(cols = all_of(ind),
               names_to = "index",
               values_to = "value")

ind<-full_join(GC_long,GS_long)
name<-list("T.."=expression(Total~system~throughput~("T..,"~mg~C~m^-2)),
           "TST"=expression(Total~system~throughflow~("TST,"~mg~C~m^-2)),
           "TSTC"=expression(Total~system~cycled~throughflow~("TSTC,"~mg~C~m^-2)),
           "FCI"="Finn cycling index(FCI)",
           "AMI"="Average mutual information(AMI)"
           
)
label <- function(variable, value){return(name[value])}

# boxplot ####
plot<-ind %>%
  ggplot(aes(x = station, y = value,color = as.factor(station)))+
  geom_boxplot(outlier.shape = NA,size=1)+
  facet_wrap(.~index,scales = "free_y",strip.position = "left",
             labeller =label)+
  theme_bw()+
  xlab("Station")+
  ylab(NULL) +
  scale_color_manual(values = c("#66C2A5","#FC8D62"))+
  theme(legend.position = "none",
        strip.text = element_text(size=15),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 12),
        title = element_text(size=25),
        axis.text = element_text(size = 15),
        plot.margin = margin(5, 5, 5, 5),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))

plot+annotation_custom(ggplotGrob(table),
                       xmin = 6, ymin = 7)
ggsave("fig/f05.png",width = 15, height =12)