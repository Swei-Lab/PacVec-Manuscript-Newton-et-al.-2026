#Thesis Project - Manuscript 2 Data Analysis 


#Set working directory
setwd("~/Documents/R-Studio/Data/2024")

#Load packages
library("sjPlot")
library("brglm2")
library("glmmTMB")
library("Rcapture")
library("ggplot2")
library("dplyr")
library("logistf")
library("tidyr")

#Figure data
Fig1 <-read.csv("NIP_23_24.csv", header = TRUE)
Fig2 <-read.csv("DIN_2023_2024.csv", header = TRUE)
Fig3 <-read.csv("MAM_MIP_AVC.csv", header = TRUE)
Fig4 <-read.csv("NIP_BINOM.csv", header = TRUE)
Fig5 <-read.csv("MAM_ABUND_NEFU_PERO.csv", header = TRUE)
Fig6 <-read.csv("Tick_Reaq.csv", header = TRUE)


#Nymphal infection prevalence: Boxplot
ggplot(Fig1, aes(x=Type, y=NIP, fill=Type)) + 
  facet_wrap(~Year) +
  xlab("") + ylab("NIP") +
  theme_minimal() +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Adds axis lines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none",
        panel.border = element_blank())


#Density of infected nymphal ticks: Boxplot
ggplot(Fig2, aes(x=Type, y=DIN, fill=Type)) + 
  facet_wrap(~Year) +
  xlab("") + ylab("DIN") +
  theme_minimal() +
  geom_boxplot() +
  ylim(0,15) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  # Adds axis lines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none",
        panel.border = element_blank()) 


#Mammal infection prevalence: Boxplot
#Boxplot:MIP by treatment type and year
ggplot(Fig3, aes(x=Treatment, y=MIP, fill=Treatment)) +
  geom_boxplot() +
  xlab("") + ylab("MIP") +
  facet_wrap(~Year) +
  theme_minimal() +
  ylim(0,60) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),  
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "none",
        panel.border = element_blank()) 


#Nymphal infection status: Prediction plot
Fig4$County <- factor(Fig4$County,
                       levels = c("San Mateo", "Marin"),
                       labels = c("Deltamethrin", "Permethrin"))
Fig_4<-subset(Fig4, Year =='2024')
Fig_4$Treatment<-as.factor(Fig_4$Treatment)
Fig_4$County<-as.factor(Fig_4$County)
Fig_4$Treatment <- relevel(factor(Fig_4$Treatment), ref = "Control")

model_county <- logistf(Infection ~ Treatment * County, data = Fig_4)
summary(model_county)

plot_model(model_county, type = "pred", terms = c("County", "Treatment"), dodge = 0.7) +
  coord_flip() +
  labs(
    x = "",
    y = "predicted probability of nymphal infection status",
    title = "Nymphal infection status") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 10, r = 10, b = 18, l = 0),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(vjust = -0.8, size = 11))


#Species Abundance 
Fig5 <- Fig5 %>%filter(Treatment == "Acaricide")

Fig5$Species_names <- ifelse(Fig5$Species == "NEFU", "N. fuscipes",
                             ifelse(Fig5$Species == "PETR", "P. truei",
                                    ifelse(Fig5$Species == "PECA", "P. californicus", 
                                           ifelse(Fig5$Species == "PEMA", "P. sonoriensis",Fig5$Species))))

Fig5_plot <- Fig5 %>%filter(!((County == "Marin" & Species_names == "P. californicus") |
             (County == "San Mateo" & Species_names == "P. sonoriensis")))

species_order <- c("N. fuscipes", "P. truei", "P. californicus", "P. sonoriensis")
Fig5_plot$Species_names <- factor(Fig5_plot$Species_names, levels = species_order)

all_combinations <- expand_grid(County = unique(Fig5_plot$County),
Species_names = species_order)

Fig5_plot <- all_combinations %>%left_join(Fig5_plot, by = c("County", "Species_names")) %>%mutate(Total = replace_na(Total, 0),SE = replace_na(SE, 0))

county_to_treatment <- c("San Mateo " = "Deltamethrin", "Marin" = "Permethrin")

ggplot(Fig5_plot, aes(x = Species_names, y = Total, fill = County)) +
  geom_col(position = "dodge", color = "black") +
  facet_wrap(~County, labeller = labeller(County = county_to_treatment)) +
  geom_errorbar(aes(ymin = Total - SE, ymax = Total + SE),
                width = 0.15,
                position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("Species abundance") +
  theme_minimal() +
  ylim(0, 65) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 60, vjust = 0.9, hjust = 0.9),
    panel.border = element_blank(),
    legend.position = "none")



#Recaptured mammal tick burden re-acquisition rate
Fig6$Species <- recode(Fig6$Species,
                        "NEFU" = "N. fuscipes",
                        "PETR" = "P. truei")
Fig6$Species<-factor(Fig6$Species)
Fig6$Treatment<-factor(Fig6$Treatment)
Fig6$Treatment <- relevel(factor(Fig6$Treatment), ref = "Permethrin")

tickmodel <- logistf(Ticks ~ Treatment * Species + Trap.day, data = Fig6)
summary(tickmodel)

plot_model(tickmodel, type = "pred", terms = c("Treatment", "Species"), dodge = 0.7) +
  coord_flip() +
  labs(
    x = "",
    y = "predicted probability of tick reacquisition",
    title = "") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = -0.5),
    plot.margin = margin(t = 10, r = 10, b = 18, l = 0),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(vjust = -0.8, size = 11))



#Statistical data
data1 <-read.csv("NIP_BINOMO.csv", header = TRUE)
data2 <-read.csv("DIN_BIN.csv", header = TRUE)
data3 <-read.csv("MIP_BINO.csv", header = TRUE)
data4 <-read.csv("MAM_ABUND_NEFU_PETR.csv", header = TRUE)
data5 <-read.csv("Tick_Reaq.csv", header = TRUE)


#GLMM: Nymphal infection prevalence (NIP) by treatment in 2023 and 2024
data1$Treatment<-as.factor(data1$Treatment)
data1$Site<-as.factor(data1$Site)
data1$Type<-as.factor(data1$Type)
data1$Infection<-as.factor(data1$Infection)
data1$Year<-factor(data1$Year)
data1$Type <- relevel(factor(data1$Type), ref = "Control")

NIP_Bin<- glmmTMB(Infection ~ Treatment * Year + (1|Type) + (1|Site), family=binomial(), data=data1)
summary(NIP_Bin)


#GLMM: Density of infected Nymphs (DIN) by treatment in 2023 and 2024 
data2$Treatment<-as.factor(data2$Treatment)
data2$Type<-as.factor(data2$Type)
data2$Infection <- as.numeric(as.character(data2$Infection))
data2$Year<-factor(data2$Year)
data2$Treatment <- relevel(factor(data2$Treatment), ref = "Control")

DIN_B<- glmmTMB(Infection ~ Treatment * Year + (1|Type) + (1|Site), family=nbinom2(), data=data2)
summary(DIN_B)


#GLMM: MIP by treatment in 2023 and 2024 
data3$Treatment<-as.factor(data3$Treatment)
data3$Type<-as.factor(data3$Type)
data3$Infection<-as.factor(data3$Infection)
data3$Year<-factor(data3$Year)
data3$Treatment <- relevel(factor(data3$Treatment), ref = "Control")

Maminf<- glmmTMB(Infection ~ Treatment * Year + (1|Type) + (1|Site), family=binomial(), data=data3)
summary(Maminf)


#Firths logic regression: Nymphal infection status (NIS) by treatment and county in 2024
data1<-subset(data1, Year =='2024')
data_01<-subset(data1, County =='San Mateo')
data_02<-subset(data1, County =='Marin')
data_01$Treatment<- relevel(factor(data_01$Treatment), ref = "Control")

#San Mateo NIS
model_01<- logistf(Infection ~ Treatment, data = data_01)
summary(model_01)

#Marin NIS
model_02<- logistf(Infection ~ Treatment, data = data_02)
summary(model_02)



#GLMM: NEFU v PETR species abundance at acaricide sites by county
data4$Total <- as.integer(data4$Total)
data4$Species<-factor(data4$Species)
data4$Site<-factor(data4$Site)
data4$Species <- relevel(factor(data4$Species), ref = "PETR")

CountyAbund <- glmmTMB(Total ~ Species * Site, family = nbinom2(), data = data4,control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(CountyAbund)


#Logistic regression: Recaptured mammal tick burden re-acquisition rate
data5$Species <- recode(data5$Species,
                                "NEFU" = "N. fuscipes",
                                "PETR" = "P. truei")
data5$Species<-factor(data5$Species)
data5$Treatment<-factor(data5$Treatment)
data5$Treatment <- relevel(factor(data5$Treatment), ref = "Permethrin")

Tick_Reacq <- logistf(Ticks ~ Treatment * Species + Trap.day, data = data5)
summary(Tick_Reacq)


#Host species abundance analysis
#Set new working directory
setwd("~/Documents/R-Studio/Data/Abundance")

#Edgewood Acaricide - Abundance
EWA1 <-read.csv("NEFU_EWA.csv", header = TRUE)
EWA2 <-read.csv("PETR_EWA.csv", header = TRUE)
EWA3 <-read.csv("PECA_EWA.csv", header = TRUE)

EWA_NEFU_MOD <-closedp.t(EWA1, dfreq=FALSE, dtype="hist")
EWA_NEFU_MOD

EWA_PETR_MOD <-closedp.t(EWA2, dfreq=FALSE, dtype="hist")
EWA_PETR_MOD

EWA_PECA_MOD <-closedp.t(EWA3, dfreq=FALSE, dtype="hist")
EWA_PECA_MOD

#Edgewood Control - Abundance
EWC1 <-read.csv("NEFU_EWC.csv", header = TRUE)
EWC2 <-read.csv("PETR_EWC.csv", header = TRUE)
EWC3 <-read.csv("PECA_EWC.csv", header = TRUE)

EWC_NEFU_MOD <-closedp.t(EWC1, dfreq=FALSE, dtype="hist")
EWC_NEFU_MOD

EWC_PETR_MOD <-closedp.t(EWC2, dfreq=FALSE, dtype="hist")
EWC_PETR_MOD

EWC_PECA_MOD <-closedp.t(EWC3, dfreq=FALSE, dtype="hist")
EWC_PECA_MOD

#Filoli Acaricide - Abundance
FLA2 <-read.csv("PETR_FLA.csv", header = TRUE)
FLA3 <-read.csv("PECA_FLA.csv", header = TRUE)


FLA_PETR_MOD <-closedp.t(FLA2, dfreq=FALSE, dtype="hist")
FLA_PETR_MOD

FLA_PECA_MOD <-closedp.t(FLA3, dfreq=FALSE, dtype="hist")
FLA_PECA_MOD


#Filoli Control - Abundance
FLC1 <-read.csv("NEFU_FLC.csv", header = TRUE)
FLC2 <-read.csv("PETR_FLC.csv", header = TRUE)
FLC3 <-read.csv("PECA_FLC.csv", header = TRUE)
FLC4 <-read.csv("PEMA_FLC.csv", header = TRUE)

FLC_NEFU_MOD <-closedp.t(FLC1, dfreq=FALSE, dtype="hist")
FLC_NEFU_MOD

FLC_PETR_MOD <-closedp.t(FLC2, dfreq=FALSE, dtype="hist")
FLC_PETR_MOD

FLC_PECA_MOD <-closedp.t(FLC3, dfreq=FALSE, dtype="hist")
FLC_PECA_MOD

FLC_PEMA_MOD <-closedp.t(FLC4, dfreq=FALSE, dtype="hist")
FLC_PEMA_MOD

#San Pedro Acaricide - Abundance
SPA1 <-read.csv("NEFU_SPA.csv", header = TRUE)
SPA2 <-read.csv("PETR_SPA.csv", header = TRUE)
SPA3 <-read.csv("PEMA_SPA.csv", header = TRUE)

SPA_NEFU_MOD <-closedp.t(SPA1, dfreq=FALSE, dtype="hist")
SPA_NEFU_MOD

SPA_PETR_MOD <-closedp.t(SPA2, dfreq=FALSE, dtype="hist")
SPA_PETR_MOD

SPA_PEMA_MOD <-closedp.t(SPA3, dfreq=FALSE, dtype="hist")
SPA_PEMA_MOD



#San Pedro Control - Abundance
SPC1 <-read.csv("NEFU_SPC.csv", header = TRUE)
SPC2 <-read.csv("PETR_SPC.csv", header = TRUE)
SPC3 <-read.csv("PEMA_SPC.csv", header = TRUE)

SPC_NEFU_MOD <-closedp.t(SPC1, dfreq=FALSE, dtype="hist")
SPC_NEFU_MOD

SPC_PETR_MOD <-closedp.t(SPC2, dfreq=FALSE, dtype="hist")
SPC_PETR_MOD

SPC_PEMA_MOD <-closedp.t(SPC3, dfreq=FALSE, dtype="hist")
SPC_PEMA_MOD
