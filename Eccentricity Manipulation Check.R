#Normality Check and Data analysis

library("lme4"); library("tidyr"); library("lmerTest");library("ggplot2"); 
library("dplyr"); library("ez")


setwd("C:/Users/newuser/Box/Basketball VR")
rm(list = ls())
list.files()

Ecc <-read.csv("./Eccentricity Check.csv", header = TRUE, sep=",",  
                 na.strings=c("NA","NaN"," ",""))

head(Ecc)

Ecc <- dplyr::filter(Ecc, !(VPN == "9" | VPN == "12" | VPN == "15" | VPN == "16"))


Ecc1 <- Ecc %>%
  group_by(VPN, Ecc) %>%
  summarise(Ecc_Cut = mean(EccCut, na.rm = TRUE))

head(Ecc1)

Ecc1$Ecc <- as.factor(Ecc1$Ecc)
Ecc1$VPN <- as.factor(Ecc1$VPN)



mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=Ecc1, REML = TRUE))
anova_stats(mod1)


P_value <- c(.085, .001, .001, .0002, .0002, .002)

p.adjust (P_value, method="holm")


mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "1" | Ecc == "2"), REML = TRUE))
anova_stats(mod1)

mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "1" | Ecc == "3"), REML = TRUE))
anova_stats(mod1)

mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "1" | Ecc == "4"), REML = TRUE))
anova_stats(mod1)

mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "2" | Ecc == "3"), REML = TRUE))
anova_stats(mod1)

mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "2" | Ecc == "4"), REML = TRUE))
anova_stats(mod1)

mod1 <- (lmer(Ecc_Cut ~ Ecc + (1|VPN), 
              data=filter(Ecc1, Ecc == "3" | Ecc == "4"), REML = TRUE))
anova_stats(mod1)





Perf_Graph <- Ecc1 %>%
  group_by(Ecc) %>%
  summarise(Total_Err = (sd(Ecc_Cut)/sqrt(n())),
            Total = (sum(Ecc_Cut)/n()))


ggplot(Perf_Graph, aes(x = Ecc, y = Total, fill = Ecc, color = Ecc))+
  geom_bar(stat = "identity", position = "dodge", aes = (fill = "grey100"))+
  geom_errorbar(aes(ymin=Total-Total_Err, ymax=Total+Total_Err), position = position_dodge(.9), width=.2)+
  coord_cartesian(ylim = c(0, 13))+
  scale_x_discrete(name = "Viewing Eccentricity")+
  scale_y_continuous(name = "Visual angle from gaze position (°)", expand = c(0,0))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12, colour="black"),
        strip.text = element_text(size = 12))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 12, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 15, b = 0),
                                    size = 12, face = "bold"))+
  theme(legend.position = "none")+
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(name="",
                     values = c("1" = "grey100",
                                "2" = "grey100",
                                "3" = "grey100",
                                "4" = "grey100"))+
  scale_color_manual(name="",
                    values = c("1" = "black",
                               "2" = "black",
                               "3" = "black",
                               "4" = "black"))
  




