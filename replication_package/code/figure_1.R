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
fem_eff <-
  rbind.data.frame(coeftest(lm(wtr_cand~treatment+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_cand~treatment+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_cand~treatment+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_cand~treatment+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_cand~treatment*female+third_round, dat), vcov = vcovHC(lm(wtr_cand~treatment*female+third_round, dat), type = "HC0"))[5,1:2],
                   coeftest(lm(wtr_engage~treatment+third_round, dat[dat$female==0,]), vcov = vcovHC(lm(wtr_engage~treatment+third_round, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_engage~treatment+third_round, dat[dat$female==1,]), vcov = vcovHC(lm(wtr_engage~treatment+third_round, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(wtr_engage~treatment*female+third_round, dat), vcov = vcovHC(lm(wtr_engage~treatment*female+third_round, dat), type = "HC0"))[5,1:2],
                   coeftest(lm(information~treatment, dat[dat$female==0,]), vcov = vcovHC(lm(information~treatment, dat[dat$female==0,]), type = "HC0"))[2,1:2],
                   coeftest(lm(information~treatment, dat[dat$female==1,]), vcov = vcovHC(lm(information~treatment, dat[dat$female==1,]), type = "HC0"))[2,1:2],
                   coeftest(lm(information~treatment*female, dat), vcov = vcovHC(lm(information~treatment*female, dat), type = "HC0"))[4,1:2])

# create figure
names(fem_eff) <- c("coef", "se")
fem_eff$category <- rep(c("Male Subsample", "Female Subsample", "Female*Treatment"), 3)
fem_eff$category <- factor(fem_eff$category, levels=c("Male Subsample", "Female Subsample", "Female*Treatment"))
fem_eff$outcome <- rep(c("WTR\n(Surveys 1-3)", "WTE\n(Surveys 1-3)", "Information\n(Surveys 1 & 2)"), each=3)
fem_eff$outcome <- factor(fem_eff$outcome, levels=c("WTR\n(Surveys 1-3)", "WTE\n(Surveys 1-3)", "Information\n(Surveys 1 & 2)"))

ggplot(data=fem_eff, aes(x=outcome, y=coef, color=category, shape=category)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=2.5, position=position_dodge(width=.5)) + 
  geom_errorbar(aes(ymin=coef-se*1.645, ymax=coef+se*1.645),width=0, size=1.2, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),width=0, position=position_dodge(width=.5)) +
  theme_bw() +
  scale_color_manual(values=c("grey70", "grey50", "grey20"), name="") +
  scale_shape_manual(values=c(16, 17, 15), name="") +
  xlab("Outcome") + ylab("Coefficient") +
  theme(text=element_text(size=16), axis.text.x = element_text(angle = 45, vjust=.9, hjust=1),
        legend.position = "bottom")
