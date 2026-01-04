#Thesis Project - Manuscript 2 Data Analysis 


#Set working directory
setwd("~/Documents/R-Studio/Data/2024")



#Figure data
Fig1 <-read.csv("NIP_23_24.csv", header = TRUE)
Fig2 <-read.csv("DIN_2023_2024.csv", header = TRUE)
Fig3 <-read.csv("MAM_MIP_AVC.csv", header = TRUE)
Fig4 <-read.csv("MAM_ABUND_NEFU_PERO.csv", header = TRUE)
Fig6 <-read.csv("Recap_Mam_Burds.csv", header = TRUE)



#Nymphal infection prevalence
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


#Density of infected nymphal ticks
ggplot(Fig2, aes(x=Type, y=DIN, fill=Type)) + 
  facet_wrap(~Year) +
  xlab("") + ylab("DIN") +
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
        panel.border = element_blank()  # Ensures only axis lines show
  ) + ylim(0,15) 


#Mammal infection prevalence
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


#Nymphal infection status
NIP <- data.frame(
  County = c("Marin County", "Marin County", "Marin County", "Marin County",
             "San Mateo County", "San Mateo County", "San Mateo County", "San Mateo County"),
  Treatment = c("2023 Control", "2024 Control", "2023 Acaricide", "2024 Acaricide",
                "2023 Control", "2024 Control", "2023 Acaricide", "2024 Acaricide"),
  Treatment_Type = c("Control", "Control", "Acaricide", "Acaricide",
                     "Control", "Control", "Acaricide", "Acaricide"),
  Prevalence = c(6, 13, 3, 0, 1, 1, 0, 6),
  SE = c(0.071, 0.044, 0.034, 0, 0.046, 0.019, 0, 0.047))

NIP_plot <- NIP %>%
  mutate(SE_plot = SE * 10)   # scale factor to make bars visible

ggplot(NIP_plot, aes(x = County, y = Prevalence, fill = Treatment)) +
  geom_bar_pattern(
    aes(pattern = Treatment),
    position = position_dodge(width = 0.8),
    stat = "identity",
    color = "black",
    width = 0.8,
    pattern_density = 0.1,
    pattern_fill = NA,
    pattern_angle = 45,
    pattern_spacing = 0.02
  ) +
  geom_errorbar(
    aes(ymin = pmax(Prevalence - SE_plot, 0),
        ymax = Prevalence + SE_plot),
    width = 0.3,
    position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#F8766D", "#00BFC4")) +
  scale_pattern_manual(values = c("none", "none", "stripe", "stripe")) +
  scale_x_discrete(labels = c("San Mateo County" = "Deltamethrin",
                              "Marin County" = "Permethrin")) +
  labs(x = "", y = "Nymphal infection status", fill = "Treatment") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    panel.border = element_blank()) + ylim(0, 15)


#Species Abundance
Fig4$Species_names <- ifelse(Fig4$Species == "NEFU", "N. fuscipes",
                             ifelse(Fig4$Species == "PETR", "P. truei",
                                    ifelse(Fig4$Species == "PECA", "P. californicus", 
                                           ifelse(Fig4$Species == "PEMA", "P. sonoriensis",Fig4$Species))))
Fig4_plot <- Fig4 %>%
  filter(!((County == "Marin" & Species_names == "P. californicus") |
             (County == "San Mateo" & Species_names == "P. sonoriensis")))

species_order <- c("N. fuscipes", "P. truei", "P. californicus", "P. sonoriensis")
Fig4_plot$Species_names <- factor(Fig4_plot$Species_names, levels = species_order)

all_combinations <- expand_grid(County = unique(Fig4_plot$County),
Species_names = species_order)

Fig4_plot <- all_combinations %>%
  left_join(Fig4_plot, by = c("County", "Species_names")) %>%
  mutate(Total = replace_na(Total, 0),
    SE = replace_na(SE, 0))

county_to_treatment <- c("San Mateo " = "Deltamethrin", "Marin" = "Permethrin")

ggplot(Fig4_plot, aes(x = Species_names, y = Total, fill = County)) +
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
Fig5$Species_names <- ifelse(Fig5$Species == "NEFU", "N. fuscipes",
                             ifelse(Fig5$Species == "PETR", "P. truei",Fig5$Species))

Fig5_plot <- Fig5 %>%
  mutate(Mean.Rate.plot = ifelse(Mean.Rate == 0, 0.001, Mean.Rate))

ggplot(Fig5_plot, aes(x=Species_names, y=Mean.Rate.plot, fill=County)) + 
  geom_col(position = "dodge", color = "black") +
  xlab("") + 
  ylab("Mean larval ticks re-acquired per day") + 
  facet_wrap(~Year) +
  scale_y_continuous(limits = c(0, 0.07), labels = function(x) ifelse(x == 0, 0, x)) +
  theme_minimal() +
  labs(fill = "Plot Location") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 14),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.border = element_blank(),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(size = 14))


#Statistical data
data1 <-read.csv("NIP_BINOM.csv", header = TRUE)
data2 <-read.csv("DIN.csv", header = TRUE)
data3 <-read.csv("MIP_BINOM.csv", header = TRUE)
data4 <-read.csv("MAM_ABUND_NEFU_PETR.csv", header = TRUE)
data5 <-read.csv("Tick_Reaq.csv", header = TRUE)


#GLMM: Nymphal infection prevalence (NIP) by treatment in 2023 and 2024
data1$Treatment<-as.factor(data1$Treatment)
data1$Infection<-as.factor(data1$Infection)
data1$Year<-factor(data1$Year)
data1$Treatment <- relevel(factor(data1$Treatment), ref = "Control")

NIP_B<- glmmTMB(Infection ~ Treatment * Year, family=binomial(), data=data1)
summary(NIP_B)


#GLMM: Density of infected Nymphs (DIN) by treatment in 2023 and 2024 
data2$Treatment<-as.factor(data2$Treatment)
data2$Infection <- as.numeric(as.character(data2$Infection))
data2$Year<-factor(data2$Year)
data2$Treatment <- relevel(factor(data2$Treatment), ref = "Control")

DIN_B<- glmmTMB(Infection ~ Treatment * Year, family=nbinom2(), data=data2)
summary(DIN_B)


#GLMM: MIP by treatment in 2023 and 2024 
data3$Treatment<-as.factor(data3$Treatment)
data3$Infection<-as.factor(data3$Infection)
data3$Year<-factor(data3$Year)
data3$Treatment <- relevel(factor(data3$Treatment), ref = "Control")

Maminf<- glmmTMB(Infection ~ Treatment * Year, family=binomial(), data=data3)
summary(Maminf)


#Firths logic regression: Nymphal infection status (NIS) by treatment and county in 2024
data_01<-subset(data1, Year =='2024')
data_01$Treatment <- relevel(factor(data_01$Treatment), ref = "Control")

model_county <- logistf(Infection ~ Treatment * County, data = data_01)
summary(model_county)


#GLMM: NEFU v PETR species abundance at acaricide sites by county
data4$Total <- as.integer(data4$Total)
data4$Species<-factor(data4$Species)
data4$Site<-factor(data4$Site)
data4$Species <- relevel(factor(data4$Species), ref = "PETR")

CountyAbund <- glmmTMB(Total ~ Species * Site, family = nbinom2(), data = data4,control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(CountyAbund)


#Logistic regression: Recaptured mammal tick burden re-acquisition rate
data5$County <- relevel(factor(data5$County), ref = "Marin")

model <- logistf(Ticks ~ County * Species + Trap.day, data = data5)
summary(model)


#Host species abundance analysis
#Set new working directory
setwd("~/Documents/R-Studio/Data/Abundance")

#Edgewood Acaricide - Abundance
EWA1 <-read.csv("NEFU_EWA.csv", header = TRUE)
EWA2 <-read.csv("PETR_EWA.csv", header = TRUE)
EWA3 <-read.csv("PECA_EWA.csv", header = TRUE)

EWA_NEFU_MOD <-closedp.t(EWA1, dfreq=FALSE, dtype="hist")

EWA_NEFU_MOD
#Abundance: 9.7 (M0)

EWA_PETR_MOD <-closedp.t(EWA2, dfreq=FALSE, dtype="hist")

EWA_PETR_MOD
#Abundance: 54.4 (Mh Chao LB)

EWA_PECA_MOD <-closedp.t(EWA3, dfreq=FALSE, dtype="hist")

EWA_PECA_MOD
#Abundance: 3.0 (Mt)

#Edgewood Control - Abundance
EWC1 <-read.csv("NEFU_EWC.csv", header = TRUE)
EWC2 <-read.csv("PETR_EWC.csv", header = TRUE)
EWC3 <-read.csv("PECA_EWC.csv", header = TRUE)

EWC_NEFU_MOD <-closedp.t(EWC1, dfreq=FALSE, dtype="hist")

EWC_NEFU_MOD
#Abundance: 3.1 (Mb)

EWC_PETR_MOD <-closedp.t(EWC2, dfreq=FALSE, dtype="hist")

EWC_PETR_MOD
#Abundance: 55.5 (Mb)

EWC_PECA_MOD <-closedp.t(EWC3, dfreq=FALSE, dtype="hist")

EWC_PECA_MOD
#Abundance: NA

#Filoli Acaricide - Abundance
FLA2 <-read.csv("PETR_FLA.csv", header = TRUE)
FLA3 <-read.csv("PECA_FLA.csv", header = TRUE)


FLA_PETR_MOD <-closedp.t(FLA2, dfreq=FALSE, dtype="hist")

FLA_PETR_MOD
#Abundance: 47.3 (Mbh)

FLA_PECA_MOD <-closedp.t(FLA3, dfreq=FALSE, dtype="hist")

FLA_PECA_MOD
#Abundance: 7.0 (Mt)


#Filoli Control - Abundance
FLC1 <-read.csv("NEFU_FLC.csv", header = TRUE)
FLC2 <-read.csv("PETR_FLC.csv", header = TRUE)
FLC3 <-read.csv("PECA_FLC.csv", header = TRUE)
FLC4 <-read.csv("PEMA_FLC.csv", header = TRUE)

FLC_NEFU_MOD <-closedp.t(FLC1, dfreq=FALSE, dtype="hist")

FLC_NEFU_MOD
#Abundance: NA

FLC_PETR_MOD <-closedp.t(FLC2, dfreq=FALSE, dtype="hist")

FLC_PETR_MOD
#Abundance: 27.0 (Mth Chao LB)

FLC_PECA_MOD <-closedp.t(FLC3, dfreq=FALSE, dtype="hist")

FLC_PECA_MOD
#Abundance: 3.0 (Mh Gamma3.5)

FLC_PEMA_MOD <-closedp.t(FLC4, dfreq=FALSE, dtype="hist")

FLC_PEMA_MOD
#Abundance: 4.2 (M0)

#San Pedro Acaricide - Abundance
SPA1 <-read.csv("NEFU_SPA.csv", header = TRUE)
SPA2 <-read.csv("PETR_SPA.csv", header = TRUE)
SPA3 <-read.csv("PEMA_SPA.csv", header = TRUE)

SPA_NEFU_MOD <-closedp.t(SPA1, dfreq=FALSE, dtype="hist")

SPA_NEFU_MOD
#Abundance: 22.3 (Mh Chao LB)

SPA_PETR_MOD <-closedp.t(SPA2, dfreq=FALSE, dtype="hist")

SPA_PETR_MOD
#Abundance: 12.6 (Mh Chao LB)

SPA_PEMA_MOD <-closedp.t(SPA3, dfreq=FALSE, dtype="hist")

SPA_PEMA_MOD
#Abundance: 40.0 (M0)



#San Pedro Control - Abundance
SPC1 <-read.csv("NEFU_SPC.csv", header = TRUE)
SPC2 <-read.csv("PETR_SPC.csv", header = TRUE)
SPC3 <-read.csv("PEMA_SPC.csv", header = TRUE)

SPC_NEFU_MOD <-closedp.t(SPC1, dfreq=FALSE, dtype="hist")

SPC_NEFU_MOD
#Abundance: 16.5 (M0)

SPC_PETR_MOD <-closedp.t(SPC2, dfreq=FALSE, dtype="hist")

SPC_PETR_MOD
#Abundance: 13.3 (M0)

SPC_PEMA_MOD <-closedp.t(SPC3, dfreq=FALSE, dtype="hist")

SPC_PEMA_MOD
#Abundance: 11.1 (M0)
