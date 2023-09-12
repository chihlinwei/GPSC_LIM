rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(pals)
library(ggtext)
library(stringr)
library(formattable)
library(kableExtra)
library(cowplot)

#####SED#####
SED<-read_xlsx("raw/SEDstock.xlsx")
GC1<-SED %>% filter(Station=="GC1") %>% data.frame()
GS1<-SED %>% filter(Station=="GS1") %>% data.frame()

# multilayer 0-5 cm
ml_0_5<- c("0-1", "1-2", "2-3", "3-4", "4-5")
#with multilayer:"OR1_1102","OR1_1114","OR1_1126"
#"0-1"+"1-2"+"2-3"+"3-4"+"4-5"+5*"9-10"=10cm sediment OC
multi<- c("OR1_1102","OR1_1114","OR1_1126")

GC1_ml_sum05<-GC1 %>% subset(Cruise %in% multi & 
                               Section %in% ml_0_5, Cruise & OC)%>% 
  group_by(Cruise) %>% 
  summarize(OC=sum(OC=OC))
GC1_ml_sum910<-GC1 %>%   filter(Section=="9-10") %>%
  group_by(Cruise) %>%   
  summarise(OC=5*OC)
GC1_ml_sum<-rbind(GC1_ml_sum05,GC1_ml_sum910) %>% group_by(Cruise) %>% 
  summarise(OC=sum(OC))

`%!in%`<- Negate(`%in%`)
#only single layer: all cruises except for "OR1_1102","OR1_1114","OR1_1126"
#10*"0-1"=10cm sediment OC
GC1_sing_sum<-subset(GC1, Cruise %!in% multi, Cruise & OC)%>%
  group_by(Cruise) %>% 
  summarize(OC=10*OC)
GC1_SEDsum<-rbind(GC1_sing_sum,GC1_ml_sum) %>% arrange(Cruise)
GC1_SEDsum<-data.frame(GC1_SEDsum,
                       Season=c("AU","AU","SP","SP","SU","AU","SP","AU","SP","SP","AU"),
                       Station="GC1")

#GS1
GS1_ml_sum05<-GS1 %>% subset(Cruise %in% multi & 
                               Section %in% ml_0_5, Cruise & OC) %>% 
  group_by(Cruise)  %>% 
  summarize(OC=sum(OC=OC))
GS1_ml_sum910<-GS1 %>% 
  filter(Section=="9-10") %>%
  group_by(Cruise) %>% 
  summarise(OC=5*OC)
GS1_ml_sum<-rbind(GS1_ml_sum05,GS1_ml_sum910) %>% group_by(Cruise) %>% 
  summarise(OC=sum(OC))

GS1_sing_sum<-subset(GS1, Cruise %!in% multi, Cruise & OC)%>%
  group_by(Cruise) %>% 
  summarize(OC=10*OC)
GS1_SEDsum<-rbind(GS1_sing_sum,GS1_ml_sum) %>% arrange(Cruise)
GS1_SEDsum<-data.frame(GS1_SEDsum,
                       Season=c("AU","SP","SP","SU","AU","AU","SP","SP","AU"),
                       Station="GS1")
SEDsum<-rbind(GC1_SEDsum,GS1_SEDsum)
#calculate mean stock 
df_sed<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(GC1_SEDsum$OC),mean(GS1_SEDsum$OC)),
  sd=c(sd(GC1_SEDsum$OC),sd(GS1_SEDsum$OC)))
#plot
GC1_SP<- GC1_SEDsum %>% filter(Season=="SP")
GC1_SU<- GC1_SEDsum$OC[GC1_SEDsum$Season=="SU"]
GC1_AU<- GC1_SEDsum %>% filter(Season=="AU")

GS1_SP<- GS1_SEDsum %>% filter(Season=="SP")
GS1_SU<- GS1_SEDsum$OC[GS1_SEDsum$Season=="SU"]
GS1_AU<- GS1_SEDsum %>% filter(Season=="AU")

Season<-data.frame(
  Station=c(rep("GC1",3),rep("GS1",3)),
  Season=c(rep(c("SP","SU","AU"),2)),
  Mean=c(mean(GC1_SP$OC),GC1_SU,mean(GC1_AU$OC),
         mean(GS1_SP$OC),GS1_SU,mean(GS1_AU$OC)),
  sd=c(sd(GC1_SP$OC),NA,sd(GC1_AU$OC),
       sd(GS1_SP$OC),NA,sd(GS1_AU$OC))
)
color<-as.vector(c(stepped(3)[-c(1:2)],stepped(11)[-c(1:10)],stepped(15)[-c(1:14)]))
Season_order <- c("SP","SU","AU") 

ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.0399"),
  Station = c("GC1","GS1"),
  x     = c(1, 2.5),
  y     = c(1, 800000))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.8077"),
  Station = c("GC1","GS1"),
  x     = c(1, 2.5),
  y     = c(1, 750000))

plot_sed<-Season%>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = Mean))+
  geom_bar(aes(fill =Season),stat = "identity",position = position_dodge(),width = 0.6)+
  ylab(expression(Sediment~(mg~C~m^-2)))+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","SU","AU"),
                    label=c("SP (Feb-Apr)","SU (Aug)","AU (Oct-Nov)"))+
  ylim(0, 75000)+ facet_wrap(~Station)+
  scale_y_continuous(labels = scales::scientific)+
  theme_bw()+
  geom_text(data=ana_text1, fontface="bold",aes(x = x, y = y, label = label),size=6)+
  geom_text(data=ana_text2, aes(x = x, y = y, label = label),size=6)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)+
  geom_hline(data = df_sed, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 1, 3, 1))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=SEDsum,aes(x=Season,y=OC), color = "darkblue",size=2,
             position = position_jitter(0.1))
#####BAC#####
BAC<-read_xlsx("raw/BAC.xlsx")

ana_text1 <- data.frame(
  label = "Habitat p=0.0005",
  x     = 2,
  y     = 80)
df_bac<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(BAC$biomass[BAC$Station=="GC1"]),mean(BAC$biomass[BAC$Station=="GS1"])),
  sd=c(sd(BAC$biomass[BAC$Station=="GC1"]),sd(BAC$biomass[BAC$Station=="GS1"])))
#plot
plot_bac<-df_bac%>% 
  ggplot(aes(x = Station, y = Mean))+
  geom_bar(aes(fill=Station),stat = "identity",position = position_dodge(),width = 0.6)+
  ylab(expression(Bacteria~(mg~C~m^-2)))+
  xlab("Station")+
  ylim(0, NA)+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  geom_text(data=ana_text1, fontface="bold",aes(x = x, y = y, label = label),size=6)+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 1, 3, 1))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=BAC,aes(x=Station,y=biomass), color = "darkblue",size=2,
             position = position_jitter(0.1))
#####MEI#####

Mei<-read_xlsx("raw/meio_biomass.xlsx")
Mei$Season[Mei$Cruise=="OR1_1114"]<-"SU"
Mei$Season[Mei$Cruise=="OR1_1126"]<-"AU"
Mei$Season[Mei$Cruise=="OR1_1128"]<-"SP"
Mei$Season[Mei$Cruise=="OR1_1132"]<-"SP"

byseason<-Mei %>% rename(OC=total_biomass) %>% 
  group_by(Season,Station) %>% 
  summarise(mean=mean(OC),
            sd=sd(OC))
bystation<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(byseason$mean[byseason$Station=="GC1"]),
         mean(byseason$mean[byseason$Station=="GS1"])),
  sd=c(sd(byseason$mean[byseason$Station=="GC1"]),
       sd(byseason$mean[byseason$Station=="GS1"])))
df_mei<-bystation


ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.0001"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.5),
  y     = c(1, 70))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.0003"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.5),
  y     = c(1, 65))

plot_mei<-byseason%>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = mean))+
  geom_bar(aes(fill =Season),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  ylab(expression(Meiofauna~(mg~C~m^-2)))+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","SU","AU"),
                    label=c("SP (Feb-Apr)","SU (Aug)","AU (Oct-Nov)"))+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  geom_text(data=ana_text1, fontface="bold",aes(x = x, y = y, label = label),size=6)+
  geom_text(data=ana_text2, fontface="bold",aes(x = x, y = y, label = label),size=6)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1)+
  geom_hline(data = bystation, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 1, 3, 1))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=Mei,aes(x=Season,y=total_biomass), color = "darkblue",size=2,
             position = position_jitter(0.1))

#####MAC#####

MAC<-read_xlsx("raw/macro_biomass.xlsx")

#outlier
MAC_out<-MAC #including outlier
GC1<-MAC %>% filter(Station=="GC1")
GC1$zscore <- abs((GC1$OC-mean(GC1$OC))/sd(GC1$OC))
GC1_out <- subset(GC1, GC1$zscore > 3)
GC1 <- subset(GC1, GC1$zscore < 3)
GC1<-GC1%>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC))

GS1<-MAC %>% filter(Station=="GS1")
GS1$zscore <- abs((GS1$OC-mean(GS1$OC))/sd(GS1$OC))
GS1_out <- subset(GS1, GS1$zscore > 3)
GS1 <- subset(GS1, GS1$zscore < 3)
GS1<-GS1%>% 
  group_by(Cruise, Habitat, Station, Deployment, Tube) %>% 
  summarise(OC = sum(OC))

out<-rbind(GC1_out,GS1_out)
MAC_out<-MAC_out %>% ggplot()+
  geom_point(aes(x=Taxon,y=OC),size=1)+
  geom_point(data=out, aes(x=Taxon,y=OC),colour = "red", size = 1)+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
MAC<-rbind(GC1,GS1)
MAC$Season[MAC$Cruise%in%c("OR1_1099","OR1_1102","OR1_1128","OR1_1190","OR1_1132")]<-"SP"
MAC$Season[MAC$Cruise%in%c("OR1_1096","OR1_1126","OR1_1151")]<-"AU"
MAC$Season[MAC$Cruise=="OR1_1114"]<-"SU"
df_mac<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(MAC$OC[MAC$Station=="GC1"]),mean(MAC$OC[MAC$Station=="GS1"])),
  sd=c(sd(MAC$OC[MAC$Station=="GC1"]),sd(MAC$OC[MAC$Station=="GS1"])))

library(pals)
GC1_SP<- MAC %>% filter(Season=="SP"&Station=="GC1")
GC1_SU<- MAC %>% filter(Season=="SU"&Station=="GC1")
GC1_AU<- MAC %>% filter(Season=="AU"&Station=="GC1")

GS1_SP<- MAC %>% filter(Season=="SP"&Station=="GS1")
GS1_SU<- MAC%>%  filter(Season=="SU"&Station=="GS1")
GS1_AU<- MAC %>% filter(Season=="AU"&Station=="GS1")
MAC_point<-rbind(GC1_SP,GC1_SU,GC1_AU,GS1_SP,GS1_SU,GS1_AU)
Season<-data.frame(
  Station=c(rep("GC1",3),rep("GS1",3)),
  Season=c(rep(c("SP","SU","AU"),2)),
  Mean=c(mean(GC1_SP$OC),mean(GC1_SU$OC),mean(GC1_AU$OC),
         mean(GS1_SP$OC),mean(GS1_SU$OC),mean(GS1_AU$OC)),
  sd=c(sd(GC1_SP$OC),sd(GC1_SU$OC),sd(GC1_AU$OC),
       sd(GS1_SP$OC),sd(GS1_SU$OC),sd(GS1_AU$OC))
)


ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.0001"),
  Station = c("GC1","GS1"),
  x     = c(1, 2.5),
  y     = c(1, 315))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.1907"),
  Station = c("GC1","GS1"),
  x     = c(1, 2.5),
  y     = c(1, 295))


plot_mac<-Season%>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = Mean))+
  geom_bar(aes(fill =Season),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  ylab(expression(Macrofauna~(mg~C~m^-2)))+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","SU","AU"),
                    label=c("SP (Feb-Apr)","SU (Aug)","AU (Oct-Nov)"))+
  facet_wrap(~Station,scales = "free_y")+
  theme_bw()+
  geom_text(data=ana_text1, fontface="bold",aes(x = x, y = y, label = label),size=6)+
  geom_text(data=ana_text2, aes(x = x, y = y, label = label),size=6)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)+
  geom_hline(data = df_mac, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 1, 3, 1))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=MAC_point,aes(x=Season,y=OC), color = "darkblue",size=2,
             position = position_jitter(0.1))
#####table#####

mean_table<-cbind(df_sed,df_bac[2:3],df_mei[2:3],df_mac[2:3])
kable(mean_table, "html",align = "c") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Sediment" = 2, "Bacteria" = 2, "Meiofauna" = 2,"Macrofauna" = 2)) 

#####plot#####
library(cowplot)
p1 <- plot_sed+xlab(NULL)
p2 <- plot_mei+xlab(NULL)
p3 <- plot_mac+xlab(NULL)
p4 <- plot_bac+xlab(NULL)

a_row<-plot_grid(
  p1+ theme(legend.position="none"),
  p4+ theme(legend.position="none"),
  align = 'vh',
  labels = c("(a)","(b)"),
  rel_widths = c(1, 1),
  hjust = -1, nrow = 1,
  axis = "b",label_size = 18)
legend <- get_legend(p2 + 
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))
b_row <- plot_grid(
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  align = 'vh',
  labels = c("(c)", "(d)"),
  rel_widths = c(1, 1),
  hjust = -1,
  nrow = 1,axis = "b",label_size = 18)

b_row<-plot_grid(b_row, legend, ncol = 1, rel_heights = c(1, .1))
plots <- align_plots(a_row,b_row, align = 'v', axis = 'l')
plot_grid(plots[[1]],plots[[2]],nrow = 2,rel_heights = c(1,1)) 
ggsave("fig/f02.png",width = 18, height =18)

#####Oxygen utilization#####
TOU<-read_xlsx("raw/TOU.xlsx")
#calculation: from In_situ_DO_flux (mmol O2/m2/d) to carbon unit(mg C/m2/d)
#respiratory quotient (RQ) of 0.85 (Rowe et al., 2008) (ratio of CO2 produced per O2 consumed)
#C:O2= 0.85 = 1 mmolO2 -> 0.85 mmolC  
#mmolC -> mgC= 12 
TOU$SCOC<-TOU$In_situ_DO_flux*(-1)*0.85*12
TOU$O2<-TOU$In_situ_DO_flux*(-1)
GC1<-TOU %>% filter(Station=="GC1")
GS1<-TOU %>% filter(Station=="GS1")

dfO2<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$O2[GC1$Season=="SP"]),mean(GS1$O2[GS1$Season=="SP"]),
         mean(GC1$O2[GC1$Season=="AU"]),mean(GS1$O2[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$O2[GC1$Season=="SP"]),sd  (GS1$O2[GS1$Season=="SP"]),
         sd  (GC1$O2[GC1$Season=="AU"]),sd  (GS1$O2[GS1$Season=="AU"])))

dfSCOC<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$SCOC[GC1$Season=="SP"]),mean(GS1$SCOC[GS1$Season=="SP"]),
         mean(GC1$SCOC[GC1$Season=="AU"]),mean(GS1$SCOC[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$SCOC[GC1$Season=="SP"]),sd  (GS1$SCOC[GS1$Season=="SP"]),
         sd  (GC1$SCOC[GC1$Season=="AU"]),sd  (GS1$SCOC[GS1$Season=="AU"])))

df_TOU<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(TOU$SCOC[TOU$Station=="GC1"]),mean(TOU$SCOC[TOU$Station=="GS1"])),
  sd=c(sd(TOU$SCOC[TOU$Station=="GC1"]),sd(TOU$SCOC[TOU$Station=="GS1"])))

Season_order <- c("SP","AU") 
color<-as.vector(c(stepped(3)[-c(1:2)],stepped(15)[-c(1:14)]))
ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.2867"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1, 120))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.9095"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1,115))

TOU_plot<-dfSCOC %>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = Mean))+
  geom_bar(aes(fill =Season),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  ylab(expression(TOU~(mg~C~m^-2~d^-1)))+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","AU"),
                    label=c("SP (Feb-Apr)","AU (Oct-Nov)"))+
  facet_wrap(~Station)+
  ylim(-5,125)+
  theme_bw()+
  geom_text(data=ana_text1,aes(x = x, y = y, label = label),size=4.2)+
  geom_text(data=ana_text2,aes(x = x, y = y, label = label),size=4.2)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_hline(data = df_TOU, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 0, 3, 0))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=TOU,aes(x=Season,y=SCOC), color = "darkblue",size=2,
             position = position_jitter(0.1))+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)

DOU<-read_xlsx("raw/DOU.xlsx")
#nmol/cm2/s -> mmol/m2/d
#1e4*3600*24/1e6 = 864
#calculation: from In_situ_DO_flux (mmol O2/m2/d) to carbon unit(mg C/m2/d)
#respiratory quotient (RQ) of 0.85 (Rowe et al., 2008) (ratio of CO2 produced per O2 consumed)
#C:O2= 0.85 = 1 mmolO2 -> 0.85 mmolC  
#mmolC -> mgC= 12 
DOU$C<-DOU$In_situ_Integrated_Prod*(-1)*864*0.85*12
DOU$O2<-DOU$In_situ_Integrated_Prod*(-1)*864
GC1<-DOU %>% filter(Station=="GC1")
GS1<-DOU %>% filter(Station=="GS1")

dfO2<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$O2[GC1$Season=="SP"]),mean(GS1$O2[GS1$Season=="SP"]),
         mean(GC1$O2[GC1$Season=="AU"]),mean(GS1$O2[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$O2[GC1$Season=="SP"]),sd  (GS1$O2[GS1$Season=="SP"]),
         sd  (GC1$O2[GC1$Season=="AU"]),sd  (GS1$O2[GS1$Season=="AU"])))

dfDOU<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$C[GC1$Season=="SP"]),mean(GS1$C[GS1$Season=="SP"]),
         mean(GC1$C[GC1$Season=="AU"]),mean(GS1$C[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$C[GC1$Season=="SP"]),sd  (GS1$C[GS1$Season=="SP"]),
         sd  (GC1$C[GC1$Season=="AU"]),sd  (GS1$C[GS1$Season=="AU"])))

df_DOU<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(DOU$C[DOU$Station=="GC1"]),mean(DOU$C[DOU$Station=="GS1"])),
  sd=c(sd(DOU$C[DOU$Station=="GC1"]),sd(DOU$C[DOU$Station=="GS1"])))


ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.1925"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1, 120))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.5836"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1,115))

DOU_plot <-dfDOU%>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = Mean))+
  geom_bar(aes(fill =Season),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","AU"),
                    label=c("SP (Feb-Apr)","AU (Oct-Nov)"))+
  facet_wrap(~Station)+
  ylab(expression(DOU~(mg~C~m^-2~d^-1)))+
  ylim(-5,125)+
  theme_bw()+
  geom_text(data=ana_text1,aes(x = x, y = y, label = label),size=4.2)+
  geom_text(data=ana_text2,aes(x = x, y = y, label = label),size=4.2)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)+
  geom_hline(data = df_DOU, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 0, 3, 0))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=DOU,aes(x=Season,y=C), color = "darkblue",size=2,
             position = position_jitter(0.1))

BOU<-read_xlsx("raw/BOU.xlsx")
BOU$C<-BOU$BMU*(-1)*0.85*12
BOU$O2<-BOU$BMU*(-1)
GC1<-BOU %>% filter(Station=="GC1") %>% filter(!is.na(O2)&!is.na(C)) %>% filter(C>0)
GS1<-BOU %>% filter(Station=="GS1") %>% filter(!is.na(O2)&!is.na(C)) %>% filter(C>0)
BOU<-BOU %>% filter(C>0 & O2>0)
dfO2<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$O2[GC1$Season=="SP"]),mean(GS1$O2[GS1$Season=="SP"]),
         mean(GC1$O2[GC1$Season=="AU"]),mean(GS1$O2[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$O2[GC1$Season=="SP"]),sd  (GS1$O2[GS1$Season=="SP"]),
         sd  (GC1$O2[GC1$Season=="AU"]),sd  (GS1$O2[GS1$Season=="AU"])))

dfBOU<-data.frame(
  Station=c("GC1","GS1","GC1","GS1"),
  Season=c("SP","SP","AU","AU"),
  Mean=c(mean(GC1$C[GC1$Season=="SP"]),mean(GS1$C[GS1$Season=="SP"]),
         mean(GC1$C[GC1$Season=="AU"]),mean(GS1$C[GS1$Season=="AU"])),
  sd=c(  sd  (GC1$C[GC1$Season=="SP"]),sd  (GS1$C[GS1$Season=="SP"]),
         sd  (GC1$C[GC1$Season=="AU"]),sd  (GS1$C[GS1$Season=="AU"])))

df_BOU<-data.frame(
  Station=c("GC1","GS1"),
  Mean=c(mean(BOU$C[BOU$Station=="GC1"]),mean(BOU$C[BOU$Station=="GS1"])),
  sd=c(sd(BOU$C[BOU$Station=="GC1"]),sd(BOU$C[BOU$Station=="GS1"])))

mean_table<-cbind(df_TOU,df_DOU[2:3],df_BOU[2:3])

kable(mean_table, "html",align = "c") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "TOU" = 2, "DOU" = 2, "BMU" = 2)) 

ana_text1 <- data.frame(
  label = c(NA,"Habitat p=0.5431"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1, 120))
ana_text2 <- data.frame(
  label = c(NA,"Season p=0.8666"),
  Station = c("GC1","GS1"),
  x     = c(1, 1.8),
  y     = c(1, 115))

BOU_plot <- dfBOU %>% 
  ggplot(aes(x = factor(Season,level=Season_order), y = Mean))+
  geom_bar(aes(fill =Season),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  ylab(expression(BMU~(mg~C~m^-2~d^-1)))+
  xlab("Season")+
  scale_fill_manual(values=color,
                    breaks=c("SP","AU"),
                    label=c("SP (Feb-Apr)","AU (Oct-Nov)"))+
  facet_wrap(~Station)+
  ylim(-5,125)+
  theme_bw()+
  geom_text(data=ana_text1,aes(x = x, y = y, label = label),size=4.2)+
  geom_text(data=ana_text2,aes(x = x, y = y, label = label),size=4.2)+
  guides(color = guide_legend(override.aes = list(size = 2) ) )+
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.1)+
  geom_hline(data = df_BOU, aes(yintercept = Mean),
             linetype=2,color="red")+
  theme(strip.text = element_text(size=20))+
  theme(legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        title = element_text(size=25),
        plot.margin = margin(3, 0, 3, 0))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_point(data=BOU,aes(x=Season,y=C), color = "darkblue",size=2,
             position = position_jitter(0.1))
p1 <- TOU_plot+xlab(NULL)
p2 <- DOU_plot+xlab(NULL)
p3 <- BOU_plot+xlab(NULL)
prow <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  align = 'vh',
  labels = c("(a)", "(b)", "(c)"),
  hjust = -1,
  nrow = 1,label_size = 18
)

legend <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("fig/f03.png",width = 18, height =9)

#####Turnover#####
#GC1####
load(file="Rdata/GC1_10000_50.Rdata")
GC1_LIM<-LIM
GC1_xs<-xs
#GS1####
load(file="Rdata/GS1_10000_50.Rdata")
GS1_LIM<-LIM
GS1_xs<-xs

GC1<-data.frame(flow=GC1_LIM$Unknowns,
                mean=round(colMeans(GC1_xs$X),3),
                station="GC1")
GS1<-data.frame(flow=GS1_LIM$Unknowns,
                mean=round(colMeans(GS1_xs$X),3),
                station="GS1")
df_lim<-rbind(GC1,GS1)
#####table#####
to<-data.frame(Station=c("GC1","GC1","GS1","GS1"),
               OCt_TOU=c(df_sed$Mean[1]/df_TOU$Mean[1]/365,
                         df_sed$Mean[1]/(df_lim$mean[10]+df_lim$mean[13]+df_lim$mean[16])/365,
                         df_sed$Mean[2]/df_TOU$Mean[2]/365,
                         df_sed$Mean[2]/(df_lim$mean[27]+df_lim$mean[30]+df_lim$mean[33])/365),
               OCb_DOU=c(df_bac$Mean[1]/df_DOU$Mean[1],
                         df_bac$Mean[1]/df_lim$mean[10],
                         df_bac$Mean[2]/df_DOU$Mean[2],
                         df_bac$Mean[2]/df_lim$mean[27]),
               OCmm_BOU=c((df_mei$Mean[1]+df_mac$Mean[1])/df_BOU$Mean[1],
                          (df_mei$Mean[1]+df_mac$Mean[1])/(df_lim$mean[13]+df_lim$mean[16]),
                          (df_mei$Mean[2]+df_mac$Mean[2])/df_BOU$Mean[2],
                          (df_mei$Mean[2]+df_mac$Mean[2])/(df_lim$mean[30]+df_lim$mean[33])),
               Method=rep(c("Measurement","Model"),2))
kable(to, "html",align = "c",
      col.names = c("Station","OCtotal/TOU (yr)","OCbac/DOU (d)","OCmei+mac/BOU (d)","Method")) %>%
  kable_styling("striped", full_width = F) 
