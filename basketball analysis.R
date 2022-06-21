#Normality Check and Data analysis

library("lme4"); library("tidyr"); library("lmerTest");library("ggplot2"); 
library("reshape2"); library("MMeM"); library("sjstats");library("ggpubr")
library("MANOVA.RM"); library("dplyr")
library("effectsize"); library("lsr"); library("MASS")


setwd("C:/Users/bdecouto/Box/Basketball VR")
rm(list = ls())
list.files()

D1_Ecc<-read.csv("./Basketball_Data.csv", header = TRUE, sep=",",  
             na.strings=c("NA","NaN"," ",""))
D1 <- D1_Ecc
D1$Ecc <- as.factor(D1$Ecc)
#remove participants who were not affected by anxiety manipulation
#D1 <- dplyr::filter(D1_Ecc, !(subID == "9" | subID == "12" | subID == "15" | subID == "16"))
tail(D1)

cor(LA$RA, HA$RA, method = "pearson")


#Looking at the shape of data with or without log transformations
D1 <- mutate(D1, RT = scale(RT, scale = FALSE))
D1 <- mutate(D1, "RA" = scale(RA, scale = FALSE))
D1$RT


hist(1/(D1$RA), breaks = 50, probability=T)
hist(D1$nBack_num,probability=T)
hist(log10(D1$gaze_dist_ball),probability=T)
hist(log10(D1$N_fix),probability=T)
hist((log10(D1$Fix_dur)),probability=T)
hist(log10(D1$Time_on_ball),probability=T)
hist(log10(D1$Eccentricity),probability=T)

#played with some variables with qqplots
ggpubr::ggqqplot((D1$gaze_dist_ball^2))
#normality test
shapiro.test((D1$RT^.5))







fit=lmer(log10(Time_on_ball) ~ Condition +
           (1|subID), data=D1)
summary(fit)
AIC(fit)
res2=residuals(fit,type="pearson")
shapiro.test(res2) # W = 0.9279, p-value = 0.03427

#Analyses
head(D1)
LA <- filter(D1, Ecc == "1" & Condition == "LA")
HA <- filter(D1, Ecc == "1" & Condition == "HA")
AA <- filter(D1, Ecc == "1")

wilcox.test(LA$Anxiety, HA$Anxiety, paired=TRUE)

t.test(LA$gaze_dist_ball, HA$gaze_dist_ball, paired = TRUE)
cohensD(LA$Time_on_ball, HA$Time_on_ball, method = "paired")

cohens_d(Mental_Effort ~ Condition, data = AA, paired = TRUE)

t.test((LA$Fix_dur), (HA$Fix_dur), paired = TRUE)
cohensD((LA$Eccentricity), (HA$Eccentricity), method = "paired")

colMeans(dplyr::select(HA, Anxiety, Mental_Effort, HR, 
                RA, RT, nBack_num, 
                Fix_dur, N_fix, gaze_dist_ball, Time_on_ball), 
         na.rm = TRUE)


apply(LA, 2, sd, na.rm = TRUE)

LA_Gaze <- D1 %>%
  group_by(subID, Condition) %>%
  filter(Condition == "LA") %>%
  summarise(RA = mean(RA),
            RT = mean(RT))
HA_Gaze <- D1 %>%
  group_by(subID, Condition) %>%
  filter(Condition == "HA") %>%
  summarise(RA = mean(RA),
            RT = mean(RT))
t.test(LA_Gaze$RT, HA_Gaze$RT, paired = TRUE)
cohensD(LA_Gaze$RT, HA_Gaze$RT, method = "paired")


#mod1 <- manova(cbind(RA, RT, nBack_num) ~ Condition*factor(Ecc) + Error(factor(subID)/Condition) + Error(factor(subID)/factor(Ecc)), data = D1)
#summary(mod1)
interAB<-interaction(D1$Condition, D1$Ecc)

kruskal.test(RA ~ interAB, data = D1)


D1$Ecc <-  as.factor(D1$Ecc)
D1$Condition <-  as.factor(D1$Condition)
D1_Ecc$Ecc <-  as.factor(D1_Ecc$Ecc)
D1_Ecc$Condition <-  as.factor(D1_Ecc$Condition)

P_value <- c(0.012, 0.254, 0.009, 0.001, 0.327, 0.000057)

p.adjust (P_value, method="bonferroni")

library("car")
bartlett.test((RA) ~ interaction(Condition,Ecc), data=D1)
leveneTest(RT ~ Condition*Ecc, data = D1)

fligner.test(RA ~ interaction(Condition,Ecc), data = D1)



mod1 <- (lmer(RT ~ Condition * Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID), data = D1))
anova_stats(mod1)
sjstats::anova_stats(mod1)

Ecc1 <- filter(D1, Ecc == "2")
Ecc2 <- filter(D1, Ecc == "4")
cohensD(Ecc1$RT, Ecc2$RT, method = "paired")


wilcox.test(Ecc1$RT, Ecc2$RT, paired=TRUE)

P_value <- c(.002, .017, .0002, .149, .00001, .0000009)

p.adjust (P_value, method="bonferroni")

      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "2")))
      sjstats::anova_stats(mod1)
      
      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "3")))     
      anova_stats(mod1)
      
      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "4")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "2" | Ecc == "3")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "2" | Ecc == "4")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RT ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "3" | Ecc == "4")))
      anova_stats(mod1)

help("isSingular")
mod1 <- (lmer(RA ~ Condition * Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID), data = D1))
anova_stats(mod1)
eta_squared(mod1)


Ecc1 <- filter(D1, Ecc == "4")
Ecc2 <- filter(D1, Ecc == "3")
cohensD(Ecc1$RA, Ecc2$RA, method = "paired")


wilcox.test(Ecc1$RA, Ecc2$RA, paired=TRUE)

P_value <- c(.00002, .0003, .000001, .0008, .331, .001)

p.adjust (P_value, method="bonferroni")




P_value <- c(.0001, .00008, .00000005, .325, .001, .00003)

p.adjust (P_value, method="holm")

      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "2")))
      anova_stats(mod1)
      AIC(mod1)
      
      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "3")))     
      anova_stats(mod1)
      
      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "1" | Ecc == "4")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "2" | Ecc == "3")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "2" | Ecc == "4")))
      anova_stats(mod1)
      
      mod1 <- (lmer(RA ~ Ecc + (1|subID) + (1|Condition:subID) + (1|Ecc:subID),
                         data = dplyr::filter(D1, Ecc == "3" | Ecc == "4")))
      anova_stats(mod1)
      
xtabs(~Ecc, D1)

 
length(D1$RT)

Plot <- D1 %>%
group_by(Ecc, Condition) %>%
summarise(RT_ = mean(RT),
          Err = sd(RT)/sqrt(n()))

Plot$Ecc <- as.factor(Plot$Ecc)
Plot$RT <- as.numeric(Plot$RT)


ggplot(Plot, aes(Ecc, RT_))+
  geom_point(aes(shape = Condition), size = 3, position = position_dodge(.5))+
  geom_errorbar(aes(ymin=RT_-Err, ymax=RT_+Err, group = Condition), 
                alpha = .5, position = position_dodge(.5), width=.2)+
  geom_line(aes(Ecc, RT_, group = Condition), 
            position = position_dodge(.5), alpha = .3, color = "black")+
  scale_y_continuous(name = "Response Time (ms)")+
  scale_x_discrete(name = "Viewing Eccentricity")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12, colour="black"),
        strip.text = element_text(size = 12))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 12, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 12, face = "bold"))+
  theme(legend.position = c(0.1,0.88),
        legend.text = element_text(size = 10))+
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_shape_manual(name="",
                    labels = c("High Pressure",
                               "Low Pressure"),
                    values = c("HA" = "triangle",
                               "LA" = "circle"))






Plot <- D1 %>%
  group_by(Ecc, Condition) %>%
  summarise(RT_ = (mean(RA))/100,
            Err = (sd(RA)/sqrt(n()))/100)

Plot$Ecc <- as.factor(Plot$Ecc)
levels(Plot$Ecc) <- c('High Anxiety', 'Low Anxiety')


ggplot(Plot, aes(Ecc, RT_))+
  geom_point(aes(shape = Condition), size = 3, position = position_dodge(.5))+
  geom_errorbar(aes(ymin=RT_-Err, ymax=RT_+Err, group = Condition), 
                alpha = .5, position = position_dodge(.5), width=.2)+
  geom_line(aes(Ecc, RT_, group = Condition), 
            position = position_dodge(.5), alpha = .3, color = "black")+
  coord_cartesian(ylim = c(.65, 1.02))+
  scale_y_continuous(name = "Response Accuracy", 
                     labels = scales::percent, breaks = c(.7, .8, .9, 1.0))+
  scale_x_discrete(name = "Viewing Eccentricity")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12, colour="black"),
        strip.text = element_text(size = 12))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 12, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 12, face = "bold"))+
  theme(legend.position = c(0.9,0.88),
        legend.text = element_text(size = 10))+
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_shape_manual(name="",
                     labels = c("High Pressure",
                                "Low Pressure"),
                     values = c("HA" = "triangle",
                                "LA" = "circle"))


      
      
      
      
      
      



mod6 <- (lmer(RT ~ Condition + (gaze_dist_ball) + Ecc +
                (1|subID) + (1|subID:Condition) + (1|subID:Ecc), data = D1))
anova_stats(mod6)
eta_sq(mod6)




P_value <- c(.061, .009, .636, .824, .211, .0002, .426, .348, .074, .044)

p.adjust (P_value, method="holm")




