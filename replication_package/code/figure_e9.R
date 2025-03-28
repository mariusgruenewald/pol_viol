rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")
library(ggplot2)
library(lmtest)
library(sandwich)

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those with high political interest
dat = dat[dat$polint>=5,]

# models
traits_eff <-
  rbind.data.frame(coeftest(lm(traits_1~treatment, data=dat), vcov = vcovHC(lm(traits_1~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_3~treatment, data=dat), vcov = vcovHC(lm(traits_3~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_4~treatment, data=dat), vcov = vcovHC(lm(traits_4~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_6~treatment, data=dat), vcov = vcovHC(lm(traits_6~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_2~treatment, data=dat), vcov = vcovHC(lm(traits_2~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_5~treatment, data=dat), vcov = vcovHC(lm(traits_5~treatment, data=dat), type = "HC0"))[2,1:2],
                   coeftest(lm(traits_7~treatment, data=dat), vcov = vcovHC(lm(traits_7~treatment, data=dat), type = "HC0"))[2,1:2])

# create figure
names(traits_eff) <- c("coef", "se")
traits_eff$trait <- c("Assertiveness", "Good public speaker", "Thick skin", "Self-promotion", "Consensus-seeking", "Social networker", "Empathy")
traits_eff$trait <- factor(traits_eff$trait, levels=c("Assertiveness", "Good public speaker", "Thick skin", "Self-promotion", "Consensus-seeking", "Social networker", "Empathy"))
traits_eff$masculine <- c(rep("'Masculine'\ntraits/skills", 4), rep("'Feminine'\ntraits/skills", 3))

ggplot(data=traits_eff, aes(x=trait, y=coef)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  theme_bw() +
  xlab("Trait") + ylab("Coefficient") +
  facet_wrap(~masculine, scales="free_x") +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=1, hjust=1))