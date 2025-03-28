rm(list=ls())
setwd("~/Dropbox/KommPolViolence/survey experiment/replication_survey")
library(ggplot2)
library(lmtest)
library(sandwich)

# laod data set
dat = read.csv("data_survey_allrounds.csv")

# restrict data to those with high political interest
dat = dat[dat$polint>=5,]

percep_eff <- rbind.data.frame(coeftest(lm(perception_envir_ind~treatment+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(perception_envir_ind~treatment+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                               coeftest(lm(perception_envir_ind~treatment+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(perception_envir_ind~treatment+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                               coeftest(lm(perception_envir_ind~treatment*female+third_round, dat), vcov = vcovHC(lm(perception_envir_ind~treatment*female+third_round, dat), type = "HC0"))[5,1:2],
                               coeftest(lm(perception_envir_ind~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(perception_envir_ind~treatment+age+factor(education)+polint+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                               coeftest(lm(perception_envir_ind~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(perception_envir_ind~treatment+age+factor(education)+polint+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                               coeftest(lm(perception_envir_ind~treatment*female+age+factor(education)+polint+third_round, dat), vcov = vcovHC(lm(perception_envir_ind~treatment*female+age+factor(education)+polint+third_round, dat), type = "HC0"))[9,1:2])

names(percep_eff) <- c("coef", "se")
percep_eff$category <- rep(c("Male Subsample", "Female Subsample", "Female*Treatment"), 2)
percep_eff$category <- factor(percep_eff$category, levels=c("Male Subsample", "Female Subsample", "Female*Treatment"))
percep_eff$covars <- rep(c("w/o covariates", "w/ covariates"), each=3)
percep_eff$covars <- factor(percep_eff$covars, levels=c("w/o covariates", "w/ covariates"))

ggplot(data=percep_eff, aes(x=category, y=coef, group=factor(covars), shape=factor(covars))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  xlab("") + ylab("Coefficient") +
  theme_bw() +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1),
        legend.position = "bottom") +
  scale_shape_manual(values=c(16, 17), name="")
